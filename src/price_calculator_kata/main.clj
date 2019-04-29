(ns price-calculator-kata.main
  (:require [clojure.data.json :as json])
  (:require [clojure.java.io :as io])
  (:require [price-calculator-kata.calculator :as calculator])
  (:require [price-calculator-kata.report :as report])
  (:gen-class))


(def product (json/read-str (slurp (io/resource "product.json")) :key-fn keyword))


(defn main
  [& args]
  (let [records
        (json/read-str (slurp (io/resource "data.json")) :key-fn keyword)
        products
        (json/read-str (slurp (io/resource "product.json")) :key-fn keyword)]
    (map
     report/report
     (map (fn [x]
            (calculator/construct-calculator x records))
          (get products :products)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args] (main))
