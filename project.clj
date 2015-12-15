(defproject clj-mapdb "0.1.0"
  :description "Idiomatic Clojure wrapper around MapDB"
  :url "https://github.com/ngrunwald/clj-mapdb"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.mapdb/mapdb "1.0.8"]
                 [potemkin "0.4.2"]
                 [im.chit/iroh "0.1.11"]]
  :profiles {:dev {:dependencies [[expectations "2.1.4"]]
                   :plugins [[lein-expectations "0.0.7"]]
                   :aot [clj-mapdb.serializers]}})
