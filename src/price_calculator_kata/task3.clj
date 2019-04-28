(require '[clojure.string :as string])

(defn round
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn tax
  ;; Retrieve tax for the given product
  ([product] (tax product 0.2))
  ([product percentage] (round 2 (* (get product :price) percentage))))

(defn discount
  [product percentage]
  (- (round 2 (* (get product :price) percentage))))

(defn report-product-price
  [product taxP discountP]
   (string/join ["Price before = $" (get product :price) ", price after = $" (reduce + [(get product :price) (tax product taxP) (discount product discountP)])]))

(defn report-price-changes
  [taxP discountP]
  (string/join ["Tax=" (* taxP 100) "%," (if (= 0 discountP) " no discount" (format " discount = %s%%" (* discountP 100)))]))

(defn report-price-changes-amount
  [product taxP discountP]
  (string/join ["Tax amount = $" (tax product taxP) ";" (if (= 0 discountP) "" (format " Discount amount = %s$" (-(discount product discountP))))]))

;; Definition of done:
;; Sample product: Title = “The Little Prince”, UPC=12345, price=$20.25.

;; Case #1:
;; Tax = 20%, discount = 15%
;; Program prints price $21.26
;; Program displays $3.04 amount which was deduced

;; Case #2:
;; Tax = 20%, no discount
;; Program prints price $24.30
;; Program doesn’t show any discounted amount.
(defn report-product-info
  ([product] (report-product-info product 0.2))
  ([product taxP] (report-product-info product taxP 0))
  ([product taxP discountP]
  [(report-price-changes taxP discountP) (report-price-changes-amount product taxP discountP) (report-product-price product taxP discountP)]))





(def product  {:name "The Little Prince" :upc 12345 :price 20.25})

(println (string/join "\n" (report-product-info product 0.2 0.15)))
(println (string/join "\n" (report-product-info product 0.2 )))

