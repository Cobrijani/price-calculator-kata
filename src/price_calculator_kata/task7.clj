;; 7. COMBINING
;; Customer is not satisfied with the way in which discounts are combined (simple sum).
;; New request is to allow the customer to select between two methods of combining discounts: (1) additive - discounts are all calculated from the original price and summed up, or (2) multiplicative - each discount is calculated from the price after applying the previous one.

;; Definition of done:
;; Sample product: Title = “The Little Prince”, UPC=12345, price=$20.25.

;; Case #1:
;; Tax = 21%, discount = 15%, UPC discount = 7% for UPC=12345, additive discounts
;; Packaging cost = 1% of price
;; Transport cost = $2.2
;; Tax amount = $20.25 * 21% = $4.25, discounts = $20.25 * 15% + $20.25 * 7% = $3.04 + $1.42 = $4.46
;; Packaging = $20.25 * 1% = $0.20, transport = $2.2

;; Program prints:
;; Cost = $20.25
;; Tax = $4.25
;; Discounts = $4.46
;; Packaging = $0.20
;; Transport = $2.2
;; TOTAL = $22.44
;; Program separately reports $4.46 total discount

;; Case #2:
;; Tax = 21%, discount = 15%, UPC discount = 7% for UPC=12345, multiplicative discounts
;; Packaging cost = 1% of price
;; Transport cost = $2.2
;; Tax amount = $20.25 * 21% = $4.25, discount #1 = $20.25 * 15% = $3.04; discount #2 = ($20.25 - $3.04) * 7% = $1.20
;; Packaging = $20.25 * 1% = $0.20, transport = $2.2

;; Program prints:
;; Cost = $20.25
;; Tax = $4.25
;; Discounts = $4.24
;; Packaging = $0.20
;; Transport = $2.2
;; TOTAL = $22.65
;; Program separately reports $4.24 total discount
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



(defrecord StandardProductCalculation [product taxP discounts expenses, discount-strategy]
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
    (+ (preTaxDiscount _) (postTaxDiscount _))
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



;; Case #1:
;; Tax = 21%, discount = 15%, UPC discount = 7% for UPC=12345, additive discounts
;; Packaging cost = 1% of price
;; Transport cost = $2.2
;; Tax amount = $20.25 * 21% = $4.25, discounts = $20.25 * 15% + $20.25 * 7% = $3.04 + $1.42 = $4.46
;; Packaging = $20.25 * 1% = $0.20, transport = $2.2

(def product {:name "The Little Prince", :upc 12345, :price 20.25})
(def discounts [(->UniversalDiscount 0.15 true)  (->UPCDiscount 12345 0.07 true)])
(def expenses [(->NormalExpense "Packaging" (* 0.01 (get product :price))) (->NormalExpense "Transport" 2.2)])
(def prod (->StandardProductCalculation product 0.21 discounts expenses additive-discount))

(println "Case #1")
(report prod expenses)

;; Program prints:
;; Cost = $20.25
;; Tax = $4.25
;; Discounts = $4.46
;; Packaging = $0.20
;; Transport = $2.2
;; TOTAL = $22.44
;; Program separately reports $4.46 total discount

;; Case #2:
;; Tax = 21%, discount = 15%, UPC discount = 7% for UPC=12345, multiplicative discounts
;; Packaging cost = 1% of price
;; Transport cost = $2.2
;; Tax amount = $20.25 * 21% = $4.25, discount #1 = $20.25 * 15% = $3.04; discount #2 = ($20.25 - $3.04) * 7% = $1.20
;; Packaging = $20.25 * 1% = $0.20, transport = $2.2
(println "Case #2")
(def prod2 (->StandardProductCalculation product 0.21 discounts expenses multiplicative-discount))
(report prod2 expenses)
;; Program prints:
;; Cost = $20.25
;; Tax = $4.25
;; Discounts = $4.24
;; Packaging = $0.20
;; Transport = $2.2
;; TOTAL = $22.65
;; Program separately reports $4.24 total discount