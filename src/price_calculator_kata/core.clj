(ns price-calculator-kata.core
  (:require [clojure.data.json :as json])
  (:require [clojure.java.io :as io])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]  (println "Hello, World!"))




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
        (min (value (cap _)) (+ x y)))
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
      (->StandardCurrency (apply + (map (fn [x] (value (cost x _))) expens)) (code (initialPrice _)))
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
(defn construct-discount
  [entry]
  (case (get entry :type)
    "universal" (->UniversalDiscount (get entry :value) (get entry :afterTax))
    "upc" (->UPCDiscount (get entry :upc) (get entry :value) (get entry :afterTax))
    ))

(defn construct-currency
  [entry]
  (->StandardCurrency (get entry :value) (get entry :code)))

(defn map-price-function
  [entry]
  (case entry
    "initialPrice" initialPrice
    "netPrice" netPrice
    "discountPrice" discountPrice
    "grossPrice" grossPrice))

(defn construct-expense
  [entry]
  (case (get entry :type)
    "fixed" (static-expense (construct-currency (get entry :cost)) (get entry :description))
    "percentual" (percentual-expense (get entry :percent) (map-price-function (get entry :of)) (get entry :description))
    ))
(defn construct-cap
  [entry]
  (case (get entry :type)
    "fixed" (fn [x] (construct-currency entry))
    "percentual" (fn [x] (applyOp (fn [y] (* (get entry :percent) y)) ((eval (get entry :of)) x)))))

(defn _construct-discount-strategy
  [entry]
  (case (get entry :method)
        "additive" additive-discount
        "multiplicative" multiplicative-discount))
(def construct-discount-strategy (fnil _construct-discount-strategy {:method "additive"}))

(defn _construct-tax
  [entry]
  (get entry :tax))
(def construct-tax (fnil _construct-tax {:tax 0.2}))


(defn construct-entries
  "Construct discounts"
  [f entries]
  (map f entries))

(def file (io/resource "data.json"))
(def records (json/read-str (slurp file)
                            :key-fn keyword))
(def product {:name "The Little Prince", :upc 12345, :price (->StandardCurrency 20.25 "USD")})
(def discountList
  (map construct-discount (get-in records [:discount :discounts])))
(def expenseList
  (map construct-expense (get records :expenses)))

(def prod (->StandardProductCalculation
           product
           (construct-tax records)
           discountList
           expenseList
           (construct-discount-strategy (get records :discount))
           (construct-cap (get records :cap)))
  )
(report prod)

;;(costs prod)
