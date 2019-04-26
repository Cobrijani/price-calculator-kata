;; 9. CURRENCY
;; Customer is happy to announce expansion to other markets.
;; New request is to support currencies other than US dollar.
;; Currencies should be indicated using ISO-3 codes (e.g. USD, GBP, JPY, etc.).



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


(defprotocol Currency
  (code [_] "Return currency ISO-3 code")
  (value [_] "Return currency amount"))


(defrecord StandardCurrency [val c]
  Currency
  (code [_] c)
  (value [_] val))


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
  (tax [_] (->StandardCurrency (calculate-tax (value (netPrice _)) taxP) (code (netPrice _))))
  (netPrice [_]
   (->StandardCurrency (- (value (initialPrice _)) (value (preTaxDiscount _))) (code (initialPrice _)))
  )
  (preTaxDiscount [_]
    (->StandardCurrency (discount-strategy (value (initialPrice _))
      (map percent (filter #(and (not(afterTax? %)) (isEligible? % _))
    discounts))) (code (initialPrice _)))
  )
  (postTaxDiscount [_]
    (->StandardCurrency
      (discount-strategy (value (netPrice _)) (map percent (filter #(and (afterTax? %) (isEligible? % _)) discounts)))
      (code (netPrice _))
    )
  )
  (discount [_]
      (if (nil? cap)
         (->StandardCurrency (+ (value (preTaxDiscount _)) (value (postTaxDiscount _))) (code (preTaxDiscount _)))
         (->StandardCurrency (min (cap _) (+ (preTaxDiscount _) (postTaxDiscount _))) (code (preTaxDiscount _)))
      )
    )
  (grossPrice [_] (->StandardCurrency (+ (value (netPrice _)) (tax _)) (code (netPrice _))))
  (discountPrice [_] (->StandardCurrency (- (value (initialPrice _)) (value (discount _))) (code (initialPrice _))))
  (costs [_] (->StandardCurrency (apply + (map cost expenses)) (code (initialPrice _))))
  (totalPrice [_] (->StandardCurrency (+ (value (discountPrice _)) (value (tax _)) (value (costs _))) (code (discountPrice _)))))

(defn report
([prod]
  (report prod []))
([prod expenses]
  (do
    (println (format "Cost = %.2f %s" (value (initialPrice prod)) (code (initialPrice prod))))
    (println (format "Tax = %.2f %s" (value (tax prod)) (code (tax prod))))
    (println (if (= 0 (value (discount prod))) "No Discounts" (format "Discounts = %.2f %s" (value (discount prod)) (code (discount prod)))))
    (if (empty? expenses)
      (println "No Additional costs")
      (apply println (map #(format "%s = %.2f %s" (description %) value (cost %) (code (initialPrice prod))) expenses))
    )
    (println (format "TOTAL = %.2f %s" (value (totalPrice prod)) (code (totalPrice prod))))
  )))


(defn perc-cap
  [prod perc]
  ())


;; Definition of done:

;; Case #1:
;; Sample product: Title = “The Little Prince”, UPC=12345, price=20.25 USD.
;; Tax = 20%, no discounts
(def product {:name "The Little Prince", :upc 12345, :price (->StandardCurrency 20.25 "USD")})
(def prod (->StandardProductCalculation product 0.2 [] [] additive-discount nil))

;; Program prints:
;; Cost = 20.25 USD
;; Tax = 4.25 USD
;; TOTAL = 24.50 USD
;; Program reports no discount
(println "Case #1")
(report prod)



;; Case #2:
;; Sample product: Title = “The Little Prince”, UPC=12345, price=17.76 GBP.
;; Tax = 20%, no discounts
(def product {:name "The Little Prince", :upc 12345, :price (->StandardCurrency 17.76 "GBP")})
(println "Case #2")
(def prod2 (->StandardProductCalculation product 0.2 [] [] additive-discount nil))
;; Program prints:
;; Cost = 17.76 GBP
;; Tax = 3.55 GBP
;; TOTAL = 21.31 GBP
;; Program reports no discount
(report prod2)

