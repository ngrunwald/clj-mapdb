(ns clj-mapdb.serializers
  (:require [clojure.edn :as edn])
  (:import [java.io DataInput DataOutput Serializable]))

(deftype EdnSerializer []
  org.mapdb.Serializer
  (serialize [this out obj]
    (.writeUTF out (pr-str obj)))
  (deserialize [this in available]
    (edn/read-string (.readUTF in)))
  (fixedSize [this] -1)
  Serializable)
