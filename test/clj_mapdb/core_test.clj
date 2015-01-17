(ns clj-mapdb.core-test
  (:require [clj-mapdb.core :refer :all]
            [expectations :refer :all]))

(def db (create-db :memory {:cache-disable true}))

(expect org.mapdb.DB db)

(def hmap (create-collection! db :hash-map "test1" {:counter-enable true}))

(expect org.mapdb.HTreeMap (.get db "test1"))
