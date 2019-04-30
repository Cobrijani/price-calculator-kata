(ns price-calculator-kata.calculator
  (:require [price-calculator-kata.cap :as cap])
  (:require [price-calculator-kata.expense :as expense])
  (:require [price-calculator-kata.discount :as discount])
  (:require [price-calculator-kata.tax :as tax])
  (:require [price-calculator-kata.product :as product])
  (:require [price-calculator-kata.core
             :refer
             [ProductCalculation
              netPrice
              initialPrice
              preTaxDiscount
              postTaxDiscount
              tax
              percent
              afterTax?
              isEligible?
              value
              discount
              code
              cost
              description
              discountPrice
              grossPrice
              costs]])
  (:require [price-calculator-kata.util :as util])
  (:require [price-calculator-kata.currency :as currency]))


(defrecord StandardProductCalculation
    [product taxP discounts expens discount-strategy cap]
  ProductCalculation
  (initialPrice [_] (get product :price))
  (upc [_] (get product :upc))
  (tax [_]
    (currency/applyOp
     (fn [x]
       (util/calculate-percentage x taxP))
     (netPrice _)))
  (netPrice [_]
    (currency/applyOp
     -
     (initialPrice _)
     (preTaxDiscount _)))
  (preTaxDiscount [_]
    (currency/applyOp
     (fn [x] (discount-strategy
              x
              (map percent
                   (filter
                    #(and
                      (not(afterTax? %))
                      (isEligible? % _))
                    discounts))))
    (initialPrice _))
  )
  (postTaxDiscount [_]
    (currency/applyOp
     (fn [x] (discount-strategy
              x
              (map
               percent
               (filter
                #(and
                  (afterTax? %)
                  (isEligible? % _))
                discounts))))
     (netPrice _))
  )
  (discount [_]
      (if (nil? cap)
        (currency/applyOp
         +
         (preTaxDiscount _)
         (postTaxDiscount _))
        (currency/applyOp (fn [x y]
                            (min
                             (value (cap _))
                             (+ x y)))
        (preTaxDiscount _)
        (postTaxDiscount _))
      )
    )
  (grossPrice [_]
    (currency/applyOp
     +
     (netPrice _)
     (tax _)))
  (discountPrice [_]
    (currency/applyOp
     -
     (initialPrice _)
     (discount _)))
  (costs [_]
    (if (empty? expens)
      (currency/->StandardCurrency
       0
       (code
        (initialPrice _)))
      (currency/->StandardCurrency
       (apply
        +
        (map
         (fn [x]
           (value
            (cost x _)))
         expens))
       (code
        (initialPrice _)))
    )
  )
  (expenses [_]
    (map
     (fn [x]
       (hash-map
        :cost (cost x _),
        :description (description x)))
     expens))
  (totalPrice [_]
    (currency/applyOp
     +
     (discountPrice _)
     (tax _)
     (costs _)))
  )


(defn construct-calculator
  [product calculations]
  (->StandardProductCalculation
   (product/construct-product product)
   (tax/construct-tax calculations)
   (map discount/construct-discount (get-in calculations [:discount :discounts]))
   (map expense/construct-expense (get calculations :expenses))
   (discount/construct-discount-strategy (get calculations :discount))
   (cap/construct-cap (get calculations :cap))
   ))
