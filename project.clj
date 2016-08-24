(defproject clj-mapdb "0.2.0-SNAPSHOT"
  :description "Idiomatic Clojure wrapper around MapDB"
  :url "https://github.com/ngrunwald/clj-mapdb"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.mapdb/mapdb "3.0.1"]
                 [potemkin "0.4.3"]
                 [im.chit/iroh "0.1.11"]]
  :profiles {:dev {:dependencies [[expectations "2.1.9"]]
                   :plugins [[lein-expectations "0.0.7"]]
                   :aot [clj-mapdb.serializers]}})
