(defproject codejam "0.1.0"
  :description "Solutions to Google Code Jam Problems"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [swissknife "1.0.0"]
                 [clj-time "0.14.4"]]
  :profiles {:dev {:dependencies [[midje "1.9.1"]]}})
