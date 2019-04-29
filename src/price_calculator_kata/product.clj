(ns price-calculator-kata.product
  (:require [price-calculator-kata.currency :as currency]))


(defn construct-product
  [entry]
  {:name (get entry :name),
   :upc (get entry :upc),
   :price (currency/construct-currency
           (get entry :price))})
