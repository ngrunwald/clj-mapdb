(ns clj-mapdb.core
  (:import [org.mapdb DBMaker DB]
           [java.io DataInput DataOutput Serializable])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [potemkin [types :refer [definterface+]]]
            [iroh.core :as iroh :refer [.?]]
            [clojure.edn :as edn]))

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

(defn make-options-table
  [klass]
  (let [mets (get-configure-methods klass)
        grp (group-by (comp keyword dasherize :name) mets)]
    grp))

(def db-maker-options (make-options-table org.mapdb.DBMaker))

(def coll-types
  {:hash-map {:ctor (fn [mdb label] (.createHashMap mdb (name label)))
              :options (make-options-table org.mapdb.DB$HTreeMapMaker)}
   :tree-map {:ctor (fn [mdb label] (.createTreeMap mdb (name label)))
              :options (make-options-table org.mapdb.DB$BTreeMapMaker)}
   :hash-set {:ctor (fn [mdb label] (.createHashSet mdb (name label)))
              :options (make-options-table org.mapdb.DB$HTreeSetMaker)}
   :tree-set {:ctor (fn [mdb label] (.createTreeSet mdb (name label)))
              :options (make-options-table org.mapdb.DB$BTreeSetMaker)}})

(defn apply-configurator!
  [f maker v]
  #spy/d v
  #spy/d f
  (if (= 1 #spy/d (count (:params f)))
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
                                   (let [param (first params)
                                         vl (first v)]
                                     (if (instance? Class param)
                                       (instance? param vl)
                                       (number? vl))))
                                 fs)]
            (when-let [f (first matching)]
              (apply-configurator! f maker v))))
        (recur (rest todo)))
      maker)))

(defn create-collection!
  [mdb collection-type label opts]
  (if-let [old-coll (.get mdb (name label))]
    old-coll
    (let [{:keys [ctor options]} #spy/d (get coll-types collection-type)
          maker (ctor mdb label)]
      (configure-maker! options maker opts)
      (.make maker))))

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
     (.make maker)))
  ([db-type other] (if (map? other) (create-db db-type nil other) (create-db other {})))
  ([db-type] (create-db db-type nil {}))
  ([] (create-db :heap nil {})))

(def edn-serializer
  (reify
    org.mapdb.Serializer
    (serialize [this out obj]
      (.writeUTF out (pr-str obj)))
    (deserialize [this in available]
      (edn/read-string (.readUTF in)))
    (fixedSize [this] -1)
    Serializable))

(definterface+ MapListener
  (add [this k new-val] "Called when a key is created")
  (remove [this k old-val] "Called when a key is deleted")
  (update [this k old-val new-val] "Called when a value is updated"))

(defn map-listener
  [f]
  (if (instance? MapListener f)
    (reify org.mapdb.Bind$MapListener
      (update [this k old-val new-val] (cond
                                         (nil? old-val) (.add f k new-val)
                                         (nil? new-val) (.remove f k old-val)
                                         :else (.update f k old-val new-val))))
    (reify org.mapdb.Bind$MapListener
      (update [this k old-val new-val] (f k old-val new-val)))))

(defn add-listener
  [coll listener]
  (.modificationListenerAdd coll listener))

(defn remove-listener
  [coll listener]
  (.modificationListenerRemove coll listener))
