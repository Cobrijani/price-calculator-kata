(defproject price-calculator-kata "0.1.0-SNAPSHOT"
  :description "Price Calculator Showcase in Clojure"
  :url "https://github.com/Cobrijani/price-calculator-kata"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/tools.logging "0.4.1"]]
  :main ^:skip-aot price-calculator-kata.main
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
