(ns clj-mapdb.core
  (:import [org.mapdb DBMaker DB Fun$Tuple2]
           [java.io DataInput DataOutput Serializable])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [potemkin [types :refer [definterface+]]]
            [iroh.core :as iroh :refer [.?]]
            [clojure.edn :as edn]))

(defn edn-serializer
  []
  (reify
    org.mapdb.Serializer
    (serialize [this out obj]
      (.writeUTF out (pr-str obj)))
    (deserialize [this in available]
      (edn/read-string (.readUTF in)))
    (fixedSize [this] -1)
    Serializable))

(defn base-serializer
  []
  (org.mapdb.SerializerBase.))

(defn base-btree-key-serializer
  []
  (org.mapdb.BTreeKeySerializer$BasicKeySerializer. (base-serializer)))

(defn edn-btree-key-serializer
  []
  (org.mapdb.BTreeKeySerializer$BasicKeySerializer. (edn-serializer)))

(def mapdb-types
  {:cache         {:ctor (fn [size] (DBMaker/newCache size)) :args ["cache size"]}
   :cache-direct  {:ctor (fn [size] (DBMaker/newCacheDirect size)) :args ["cache size"]}
   :file          {:ctor (fn [file] (DBMaker/newFileDB (io/file file))) :args ["file path"]}
   :heap          {:ctor #(DBMaker/newHeapDB)}
   :memory        {:ctor #(DBMaker/newMemoryDB)}
   :memory-direct {:ctor #(DBMaker/newMemoryDirectDB)}
   :temp-file     {:ctor #(DBMaker/newTempFileDB)}})

(defn dasherize
  [s]
  (-> s
      (str/trim)
      (str/replace #"([a-z])([A-Z]{1,}?)([A-Z])([a-z])"
                   (fn [[_ a b c d]] (str a "-" (str/lower-case b) "-" (str/lower-case c) d)))
      (str/replace #"([A-Z])([a-z])"
                   (fn [[_ a b]] (str "-" (str/lower-case a) b)))))

(defn get-configure-methods
  [klass]
  (filter #(and
            (not (contains? (:modifiers %) :static))
            (not (contains? (:modifiers %) :protected))
            (not (contains? (:modifiers %) :constructor))
            (not (.startsWith (:name %) "_"))
            (= (:type %) klass))
          (.? klass)))

(defn coll->iterator
  [coll]
  (if (instance? java.util.Iterator coll)
    coll
    (let [tuples (map (fn [[k v]] (Fun$Tuple2. k v)) (seq coll))]
      (.iterator tuples))))

(defn make-options-table
  [klass]
  (let [mets (get-configure-methods klass)
        grp (group-by (comp keyword dasherize :name) mets)]
    grp))

(def db-maker-options (make-options-table org.mapdb.DBMaker))

(def coll-types
  {:hash-map       {:ctor (fn [mdb label] (.createHashMap mdb (name label)))
                    :options (make-options-table org.mapdb.DB$HTreeMapMaker)}
   :tree-map       {:ctor (fn [mdb label] (.createTreeMap mdb (name label)))
                    :options (make-options-table org.mapdb.DB$BTreeMapMaker)
                    :finalizers {:long (fn [mkr] (.makeLongMap mkr))
                                 :string (fn [mkr] (.makeStringMap mkr))}
                    :formatters {:pump-source coll->iterator}}
   :hash-set       {:ctor (fn [mdb label] (.createHashSet mdb (name label)))
                    :options (make-options-table org.mapdb.DB$HTreeSetMaker)}
   :tree-set       {:ctor (fn [mdb label] (.createTreeSet mdb (name label)))
                    :options (make-options-table org.mapdb.DB$BTreeSetMaker)}
   :int            {:ctor (fn [mdb label {:keys [init]}] (.createAtomicInteger mdb (name label) init))}
   :long           {:ctor (fn [mdb label {:keys [init]}] (.createAtomicLong mdb (name label) init))}
   :bool           {:ctor (fn [mdb label {:keys [init]}] (.createAtomicBoolean mdb (name label) init))}
   :string         {:ctor (fn [mdb label {:keys [init]}] (.createAtomicString mdb (name label) init))}
   :var            {:ctor (fn [mdb label {:keys [init serializer]
                                          :or {serializer (edn-serializer)}}]
                            (.createAtomicVar mdb (name label) init serializer))}
   :queue          {:ctor (fn [mdb label {:keys [serializer locks]
                                          :or {serializer (edn-serializer)
                                               locking? false}}]
                            (.createQueue mdb (name label) serializer locks))}
   :stack          {:ctor (fn [mdb label {:keys [serializer locks]
                                          :or {serializer (edn-serializer)
                                               locking? false}}]
                            (.createStack mdb (name label) serializer locks))}
   :circular-queue {:ctor (fn [mdb label {:keys [serializer size]
                                          :or {serializer (edn-serializer)
                                               locking? false}}]
                            (.createCircularQueue mdb (name label) serializer size))}})

(defn apply-configurator!
  [f maker v]
  (if (= 1 (count (:params f)))
    (when v (f maker))
    (if (sequential? v)
      (apply f maker v)
      (f maker v))))

(defn configure-maker!
  [refs maker opts]
  (loop [todo opts]
    (if-let [[k v] (first todo)]
      (let [fs (get refs (keyword k))]
        (if (= 1 (count fs))
          (apply-configurator! (first fs) maker v)
          (let [matching (filter (fn [{:keys [params]}]
                                   (let [param (second params)
                                         args-count (if (sequential? v) (count v) 1)
                                         vl (if (sequential? v) (first v) v)]
                                     (if (instance? Class param)
                                       (and (instance? param vl) (= args-count (dec (count params))))
                                       (number? vl))))
                                 fs)]
            (when-let [f (first matching)]
              (apply-configurator! f maker v))))
        (recur (rest todo)))
      maker)))

(defn create-collection!
  ([mdb collection-type label opts]
   (if-let [old-coll (.get mdb (name label))]
     old-coll
     (let [{:keys [ctor options finalizers formatters]} (get coll-types collection-type)
           finalizer (or
                      (first
                       (for [[k v] opts
                             :let [finalizer (get finalizers k)]
                             :when finalizer]
                         finalizer))
                      (fn [mkr] (.make mkr)))
           formatted-opts (reduce
                           (fn [acc [k v]]
                             (if-let [formatter (get formatters k)]
                               (assoc acc k (formatter v))
                               acc))
                           opts opts)]
       (if options
         (let [maker (ctor mdb label)]
           (configure-maker! options maker formatted-opts)
           (finalizer maker))
         (ctor mdb label formatted-opts)))))
  ([mdb collection-type label] (create-collection! mdb collection-type label {})))

(defn create-db
  ([db-type arg opts]
   (let [{:keys [ctor args]} (mapdb-types (keyword db-type))
         arity (count args)
         maker (if (= 1 arity)
                 (if (nil? arg)
                   (throw (ex-info (format "Missing argument for type %s => %s" db-type (first args))
                                   {:db-type db-type :arg arg :options opts}))
                   (ctor arg))
                 (ctor))]
     (configure-maker! db-maker-options maker opts)
     (if (:transactional? opts)
       (.makeTxMaker maker)
       (.make maker))))
  ([db-type other] (if (map? other) (create-db db-type nil other) (create-db other {})))
  ([db-type] (create-db db-type nil {}))
  ([] (create-db :heap nil {})))

(definterface+ MapListener
  (add    [this k new-val] "Called when a key is created")
  (delete [this k old-val] "Called when a key is deleted")
  (update [this k old-val new-val] "Called when a value is updated"))

(defn map-listener
  [f]
  (if (instance? MapListener f)
    (reify org.mapdb.Bind$MapListener
      (update [this k old-val new-val] (cond
                                         (nil? old-val) (.add f k new-val)
                                         (nil? new-val) (.delete f k old-val)
                                         :else (.update f k old-val new-val))))
    (reify org.mapdb.Bind$MapListener
      (update [this k old-val new-val] (f k old-val new-val)))))

(defn add-listener
  [coll listener]
  (.modificationListenerAdd coll listener))

(defn remove-listener
  [coll listener]
  (.modificationListenerRemove coll listener))

(defmacro with-tx
  [[tx tx-maker] & body]
  `(let [~(symbol tx) (.makeTx ~tx-maker)]
     (try
       (let [return# ~@body]
         (.commit ~(symbol tx))
         return#)
       (catch Exception e#
         (.rollback ~(symbol tx))
         (throw e#))
       (finally
         (.close ~(symbol tx))))))
