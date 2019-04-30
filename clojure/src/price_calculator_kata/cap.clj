(ns price-calculator-kata.cap
  (:require [price-calculator-kata.core :as core])
  (:require [price-calculator-kata.currency :as currency]))


(defn construct-cap
  [entry]
  (case (get entry :type)
    "fixed" (fn [x] (currency/construct-currency entry))
    "percentual" (fn [x]
                   (currency/applyOp
                    (fn [y]
                      (*
                       (get entry :percent) y))
                    ((core/map-price-function (get entry :of)) x)))))
