;; 8.	CAP
;; Customer is not satisfied with total discounted amount and wants to put a cap on it.
;; Cap is either a percentage of original price or an absolute amount. Either way, the discounted amount must not be larger than indicated by the cap.

;; Definition of done:
;; Sample product: Title = “The Little Prince”, UPC=12345, price=$20.25.

;; Case #1:
;; Tax = 21%, discount = 15%, UPC discount = 7% for UPC=12345, additive discounts, cap = 20%
;; Tax amount = $20.25 * 21% = $4.25, discounts = $20.25 * 15% + $20.25 * 7% = $3.04 + $1.42 = $4.46, cap = $20.25 * 20% = $4.05

;; Program prints:
;; Cost = $20.25
;; Tax = $4.25
;; Discounts = $4.05
;; TOTAL = $20.45
;; Program separately reports $4.05 total discount

;; Case #2:
;; Tax = 21%, discount = 15%, UPC discount = 7% for UPC=12345, additive discounts, cap = $4
;; Tax amount = $20.25 * 21% = $4.25, discounts = $20.25 * 15% + $20.25 * 7% = $3.04 + $1.42 = $4.46, cap = $4

;; Program prints:
;; Cost = $20.25
;; Tax = $4.25
;; Discounts = $4.00
;; TOTAL = $20.50
;; Program separately reports $4.00 total discount

;; Case #3:
;; Tax = 21%, discount = 15%, UPC discount = 7% for UPC=12345, additive discounts, cap = 30%
;; Tax amount = $20.25 * 21% = $4.25, discounts = $20.25 * 15% + $20.25 * 7% = $3.04 + $1.42 = $4.46, cap = $20.25 * 30% = $6.08

;; Program prints:
;; Cost = $20.25
;; Tax = $4.25
;; Discounts = $4.46
;; TOTAL = $20.04
;; Program separately reports $4.46 total discount
(defn round
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))
(defn calculate-percentage
  [value percent]
  (round 2 (* value percent)))

(defn calculate-tax
  [price percent]
  (calculate-percentage price percent))

(defn calculate-discount
  [price percent]
  (- (calculate-percentage price percent)))


(defprotocol ProductCalculation
  (initialPrice [_] "Return initial price")
  (productName [_] "Return product name")
  (upc [_] "Return product upc")
  (tax [_] "Return the tax amount")
  (netPrice [_] "Return price before tax")
  (preTaxDiscount [_] "Return pre tax discount for the product")
  (postTaxDiscount [_] "Return post tox discount for the product")
  (discount [_] "Return discount value for the product")
  (grossPrice [_] "return price with tax")
  (discountPrice [_]"return price with discounts")
  (costs [_] "return additional not taxed costs")
  (totalPrice [_] "return total price with tax and discounts"))

(defprotocol Discount
  (isEligible? [_ _] "checks if product is eligible for a discount")
  (percent [_] "returns discount percent")
  (discountName [_] "returns discount name")
  (afterTax? [_] "whether after or before tax price"))


(defprotocol Expense
  (cost [_] "return expense amount")
  (description [_] "return expense description"))

(defrecord NormalExpense [desc expense]
  Expense
  (cost [_] expense)
  (description [_] desc))

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
    (let [[f & other] discounts ]
        (+ (calculate-percentage price f) (multiplicative-discount (- price (calculate-percentage price f)) other)))))

(defn additive-discount
  [price discounts]
  (if (empty? discounts)
  0
  (reduce + (map #(calculate-percentage price %) discounts))))



(defrecord StandardProductCalculation [product taxP discounts expenses discount-strategy cap]
  ProductCalculation
  (initialPrice [_] (:price product))
  (upc [_] (:upc product))
  (tax [_] (calculate-tax (netPrice _) taxP))
  (netPrice [_]
   (- (initialPrice _) (preTaxDiscount _)))
  (preTaxDiscount [_]
    (discount-strategy (initialPrice _)
      (map percent (filter #(and (not(afterTax? %)) (isEligible? % _))
    discounts)))
  )
  (postTaxDiscount [_]
    (discount-strategy (netPrice _)
      (map percent (filter #(and (afterTax? %) (isEligible? % _))
    discounts)))
  )
  (discount [_]
    (min (cap _) (+ (preTaxDiscount _) (postTaxDiscount _)))
  )
  (grossPrice [_] (+ (netPrice _) (tax _)))
  (discountPrice [_] (- (initialPrice _) (discount _)))
  (costs [_] (apply + (map cost expenses)))
  (totalPrice [_] (+ (discountPrice _) (tax _) (costs _))))

(defn report
([prod]
  (report prod []))
([prod expenses]
  (do
    (println (format "Cost = $%.2f" (initialPrice prod)))
    (println (format "Tax = $%.2f" (tax prod)))
    (println (if (= 0 (discount prod)) "No Discounts" (format "Discounts = $%s" (discount prod))))
    (if (empty? expenses)
      (println "No Additional costs")
      (apply println (map #(format "%s = $%.2f" (description %) (cost %)) expenses))
    )
    (println (format "TOTAL = $%.2f" (totalPrice prod)))
  )))


(defn perc-cap
  [prod perc]
  ())


;; Definition of done:
;; Sample product: Title = “The Little Prince”, UPC=12345, price=$20.25.

;; Case #1:
;; Tax = 21%, discount = 15%, UPC discount = 7% for UPC=12345, additive discounts, cap = 20%
;; Tax amount = $20.25 * 21% = $4.25, discounts = $20.25 * 15% + $20.25 * 7% = $3.04 + $1.42 = $4.46, cap = $20.25 * 20% = $4.05

(def product {:name "The Little Prince", :upc 12345, :price 20.25})
(def discounts [(->UniversalDiscount 0.15 true)  (->UPCDiscount 12345 0.07 true)])
(def expenses [(->NormalExpense "Packaging" (* 0.01 (get product :price))) (->NormalExpense "Transport" 2.2)])
(def prod (->StandardProductCalculation product 0.21 discounts [] additive-discount (fn [p] (* 0.2 (initialPrice p)))))

;; Program prints:
;; Cost = $20.25
;; Tax = $4.25
;; Discounts = $4.05
;; TOTAL = $20.45
;; Program separately reports $4.05 total discount
(println "Case #1")
(report prod)

;; Case #2:
;; Tax = 21%, discount = 15%, UPC discount = 7% for UPC=12345, additive discounts, cap = $4
;; Tax amount = $20.25 * 21% = $4.25, discounts = $20.25 * 15% + $20.25 * 7% = $3.04 + $1.42 = $4.46, cap = $4

(println "Case #2")
(def prod2 (->StandardProductCalculation product 0.21 discounts [] additive-discount (fn [p] 4)))

;; Program prints:
;; Cost = $20.25
;; Tax = $4.25
;; Discounts = $4.00
;; TOTAL = $20.50
;; Program separately reports $4.00 total discount
(report prod2)

;; Case #3:
;; Tax = 21%, discount = 15%, UPC discount = 7% for UPC=12345, additive discounts, cap = 30%
;; Tax amount = $20.25 * 21% = $4.25, discounts = $20.25 * 15% + $20.25 * 7% = $3.04 + $1.42 = $4.46, cap = $20.25 * 30% = $6.08
(println "Case #3")
(def prod3 (->StandardProductCalculation product 0.21 discounts [] additive-discount (fn [p] (* 0.3 (initialPrice p)))))
(report prod3)
;; Program prints:
;; Cost = $20.25
;; Tax = $4.25
;; Discounts = $4.46
;; TOTAL = $20.04
;; Program separately reports $4.46 total discount