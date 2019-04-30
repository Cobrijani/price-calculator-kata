(ns price-calculator-kata.currency
  (:require [price-calculator-kata.core :refer [Currency value code]]))



(defrecord StandardCurrency [val c]
  Currency
  (code [_] c)
  (value [_] val))

(defn applyOp
  ([op x]
   (->StandardCurrency (op
                        (value x))
                       (code x)))
  ([op x y]
   (->StandardCurrency (op
                        (value x)
                        (value y))
                       (code x)))
  ([op x y & more]
   (->StandardCurrency (reduce
                        op
                        (op
                         (value x)
                         (value y))
                        (map value more))
                       (code x))))


(defn construct-currency
  [entry]
  (->StandardCurrency (get entry :value) (get entry :code)))
