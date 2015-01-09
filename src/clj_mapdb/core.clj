(ns clj-mapdb.core
  (:import [org.mapdb DBMaker DB])
  (:require [clojure.java.io :as io]
            [potemkin [types :refer [definterface+]]]
            [clojure.string :as str]
            [iroh.core :as iroh :refer [.?]]))

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
      (str/replace #"([a-z])([A-Z]{1,}?)([A-Z])([a-z])" (fn [[_ a b c d]] (str a "-" (str/lower-case b) "-" (str/lower-case c) d)))
      (str/replace #"([A-Z])([a-z])" (fn [[_ a b]] (str "-" (str/lower-case a) b)))))

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
  (if (= 1 (count (:params f)))
    (when v (f maker))
    (apply f maker v)))

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

(definterface+ IMapDB
  (db        [this] "Returns the underlying db")
  (close!    [this] "Closes this db")
  (closed?   [this] "Returns whether this db is closed")
  (commit!   [this] "Commits all pending changes on this db")
  (rollback! [this] "Rollbacks all pending changes on this db")
  (compact!  [this] "Compacts this DB to reclaim space")
  (rename!   [this old-name new-name] "Renames this collection")
  (snapshot  [this] "Returns a read-only snapshot view of this db")
  (storage   [this] "Returns the storage type for this db")
  (options   [this] "Returns the options this db was created with"))

(defn create-collection!
  [mdb collection-type label opts]
  (if-let [old-coll (get mdb (name label))]
    old-coll
    (let [{:keys [ctor options]} (get coll-types collection-type)
          maker (ctor (db mdb) label)]
      (configure-maker! options maker opts)
      (.make maker))))

(deftype MapDB [db storage options]
  IMapDB
  (db        [this] db)
  (close!    [this] (.close db) this)
  (closed?   [this] (.isClosed db))
  (commit!   [this] (.commit db) this)
  (rollback! [this] (.rollback db) this)
  (compact!  [this] (.compact db) this)
  (storage   [this] storage)
  (options   [this] options)
  clojure.lang.Counted
  (count [this]
    (count (.getAll db)))
  clojure.lang.ILookup
  (valAt [this k] (.get db (name k)))
  (valAt [this k opts] (create-collection! this (:type opts) (name k) opts))
  clojure.lang.ITransientMap
  (assoc [this k opts] (create-collection! this (:type opts) (name k) opts) this)
  (without [this k] (.delete db (name k)) this)
  clojure.lang.Seqable
  (seq [this]
    (seq (.getAll db)))
  clojure.lang.IFn
  (invoke [this k]
    (get this (name k))))

(defmethod print-method MapDB
  [db ^java.io.Writer w]
  (.write w "#MapDB<")
  (.write w (name (storage db)))
  (.write w ">{")
  (let [all-colls (for [[k v] db]
                    (str k " <" (last (str/split (str (type v)) #"\.")) ">"))]
    (.write w (str/join ", " all-colls)))
  (.write w "}"))

(defn mapdb
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
     (->MapDB (.make maker) db-type opts)))
  ([db-type other] (if (map? other) (mapdb db-type nil other) (mapdb other {})))
  ([db-type] (mapdb db-type nil {}))
  ([] (mapdb :heap nil {})))
