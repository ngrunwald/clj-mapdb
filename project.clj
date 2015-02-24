(defproject clj-mapdb "0.1.0-beta2-SNAPSHOT"
  :description "Idiomatic Clojure wrapper around MapDB"
  :url "https://github.com/ngrunwald/clj-mapdb"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.mapdb/mapdb "1.0.7"]
                 [potemkin "0.3.11"]
                 [im.chit/iroh "0.1.11"]]
  :profiles {:dev {:dependencies [[expectations "2.0.13"]]
                   :plugins [[lein-expectations "0.0.7"]]
                   :aot [clj-mapdb.serializers]}})
