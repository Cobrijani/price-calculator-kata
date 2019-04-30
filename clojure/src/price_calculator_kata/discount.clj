(ns price-calculator-kata.discount
  (:require [price-calculator-kata.core
             :refer
             [Discount
              upc]])
  (:require [price-calculator-kata.util :as util]))



(defrecord UniversalDiscount [discountP afTax?]
  Discount
  (isEligible? [_ product] true)
  (percent [_] discountP)
  (discountName [_] "Universal Discount")
  (afterTax? [_] afTax?))

(defrecord UPCDiscount [upcCode discountP afTax?]
  Discount
  (isEligible? [_ product] (= upcCode (upc product)))
  (percent [_] discountP)
  (discountName [_] "UPC Discount")
  (afterTax? [_] afTax?))



(defn multiplicative-discount
  [price discounts]
  (if (empty? discounts)
    0
    (let
        [[f & other] discounts ]
      (+
       (util/calculate-percentage price f)
       (multiplicative-discount
        (- price
           (util/calculate-percentage price f))
        other)))))

(defn additive-discount
  [price discounts]
  (if (empty? discounts)
  0
  (reduce +
          (map #(util/calculate-percentage price %)
               discounts))))


(defn _construct-discount-strategy
  [entry]
  (case (get entry :method)
    "additive" additive-discount
    "multiplicative" multiplicative-discount))

(def construct-discount-strategy
  (fnil _construct-discount-strategy
        {:method "additive"}))


(defn construct-discount
  [entry]
  (case (get entry :type)
    "universal" (->UniversalDiscount
                 (get entry :value)
                 (get entry :afterTax))
    "upc" (->UPCDiscount
           (get entry :upc)
           (get entry :value)
           (get entry :afterTax))
    ))
