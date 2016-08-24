(ns clj-mapdb.core-test
  (:require [clj-mapdb.core :refer :all]
            [expectations :refer :all]))

(def db (create-db :memory {:cache-disable true}))

(expect org.mapdb.DB db)

(def hmap (create-collection! db :tree-map "test1" {:counter-enable true}))

(expect org.mapdb.BTreeMap (.get db "test1"))

(def tx-mkr (create-db :memory {:cache-disable true :fully-transactional? true}))

(expect {:foo 42} (do (create-collection! db :tree-map "test1" {:counter-enable true})
                      (let [coll (.get db "test1")]
                        (.put coll :foo 42)
                        (into {} coll))))

;; (expect Exception (with-tx [tx2 tx-mkr]
;;                     (let [coll (.get tx2 "test1")]
;;                       (.put coll "bar" 84))
;;                     (throw (Exception.))))

(expect 1 (let [coll (.get db "test1")]
            (count (.keySet coll))))

;; (def mirror (create-collection! db :tree-map "mirror1"))
;; (bind :secondary-value hmap mirror (fn [old new] (inc new)))
;; (.put hmap "test" 42)
;; (expect 43 (.get mirror "test"))
