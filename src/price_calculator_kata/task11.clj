;; 11.	CONFIGURATOR (bonus level)
;; Customer wants to be able to configure all elements of the price calculator: tax, discounts, combination method, cap, expenses.
;; You are free to choose the configuration method – plain text file, XML file, JSON file, database, etc.
(require '[clojure.data.json :as json])



(defn round
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))
(defn calculate-percentage
  [value percent]
  (round 4 (* value percent)))

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
  (expenses [_] "return list of additional expenses")
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

(defn applyOp
  ([op x]
    (->StandardCurrency (op (value x)) (code x)))
  ([op x y]
    (->StandardCurrency (op (value x) (value y)) (code x)))
  ([op x y & more]
    (->StandardCurrency (reduce op (op (value x) (value y)) (map value more)) (code x))))


(defprotocol Expense
  (cost [_ _] "return expense amount")
  (description [_] "return expense description"))

(defrecord NormalExpense [desc expense]
  Expense
  (cost [_ prod] (expense prod) )
  (description [_] desc))

(defn static-expense
  ([amount]
  (static-expense amount "No Description"))
  ([amount desc]
  (->NormalExpense desc (fn [x] amount)))
)

(defn percentual-expense
  ([percent price]
    (percentual-expense percent price "No Description"))
  ([percent price desc]
  (->NormalExpense
    desc
    (fn [x] (->StandardCurrency (* percent (value (price x))) (code (price x))))
  ))
)

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


(defrecord StandardProductCalculation [product taxP discounts expens discount-strategy cap]
  ProductCalculation
  (initialPrice [_] (:price product))
  (upc [_] (:upc product))
  (tax [_] (applyOp (fn [x] (calculate-tax x taxP)) (netPrice _)))
  (netPrice [_] (applyOp - (initialPrice _) (preTaxDiscount _)))
  (preTaxDiscount [_]
    (applyOp (fn [x] (discount-strategy x (map percent (filter #(and (not(afterTax? %)) (isEligible? % _)) discounts))))
    (initialPrice _))
  )
  (postTaxDiscount [_]
    (applyOp (fn [x] (discount-strategy x (map percent (filter #(and (afterTax? %) (isEligible? % _)) discounts)))) (netPrice _))
  )
  (discount [_]
      (if (nil? cap)
        (applyOp + (preTaxDiscount _) (postTaxDiscount _))
        (applyOp (fn [x y]
        (min (cap _) (+ x y)))
        (preTaxDiscount _)
        (postTaxDiscount _))
      )
    )
  (grossPrice [_]
    (applyOp + (netPrice _) (tax _)))
  (discountPrice [_]
    (applyOp  - (initialPrice _) (discount _)))
  (costs [_]
    (if (empty? expens)
      (->StandardCurrency 0 (code (initialPrice _)))
      (apply #(applyOp + %) (map #(cost % _) expens))
    )
  )
  (expenses [_] (map (fn [x] (hash-map :cost (cost x _), :description (description x))) expens))
  (totalPrice [_] (applyOp + (discountPrice _) (tax _) (costs _)))
)

(defn report
[prod]
  (do
    (println (format "Cost = %.2f %s" (value (initialPrice prod)) (code (initialPrice prod))))
    (println (format "Tax = %.2f %s" (value (tax prod)) (code (tax prod))))
    (println (if (= 0 (value (discount prod))) "No Discounts" (format "Discounts = %.2f %s" (value (discount prod)) (code (discount prod)))))
    (if (empty? (expenses prod))
      (println "No Additional costs")
      (apply println
        (map
        #(format "%s = %.2f %s" (get % :description) (value (get % :cost)) (code (get % :cost)))
        (expenses prod)
        )
      )
    )
    (println (format "TOTAL = %.2f %s" (value (totalPrice prod)) (code (totalPrice prod))))
  ))



;; Definition of done:
;; Sample product: Title = “The Little Prince”, UPC=12345, price=20.25 USD.
;; Tax = 21%, universal discount = 15%, UPC discount = 7% for UPC=12345, discounts are multiplicative and after tax
;; Transport cost = 3% of base price
;; (def product {:name "The Little Prince", :upc 12345, :price (->StandardCurrency 20.25 "USD")})
;; (def discounts [(->UniversalDiscount 0.15 true) (->UPCDiscount 12345 0.07 true)])
;; (def exp1 (percentual-expense 0.03 initialPrice "Transport cost"))
;; (def exp [exp1])
;; (def prod (->StandardProductCalculation product 0.21 discounts exp multiplicative-discount nil))
;; (println "Case #1")

;; (report prod)
;; Tax amount = 20.25 USD * 21% = 4.2525 USD
;; Universal discount = 20.25 USD * 15% = 3.0375 USD
;; UPC discount = (20.25 USD – 3.0375 USD) * 7% = 1.2049 USD
;; Total discount = 3.0375 USD + 1.2049 USD = 4.2424 USD
;; Transport cost = 20.25 USD * 3% = 0.6075 USD


(def all-records (json/read-str (slurp "./product.json")
                :key-fn keyword))

(println all-records)
