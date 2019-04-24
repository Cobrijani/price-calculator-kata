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



;; Definition of done:
;; Sample product: Book with name = “The Little Prince”, UPC=12345, price=$20.25.

;; Tax=20%, discount=15%
;; Tax amount = $4.05; Discount amount = $3.04
;; Price before = $20.25, price after = $21.26
(defn report-product-info
  ([product] (report-product-info product 0.2))
  ([product taxP discountP]
  (do (println (string/join ["Tax=" (* taxP 100) "%, discount=" (* discountP 100) "%"]))
   (println (string/join ["Tax amount = $" (tax product taxP) "; Discount amount = $" (-(discount product discountP))]))
   (println (string/join ["Price before = $" (get product :price) ", price after = $" (reduce + [(get product :price) (tax product taxP) (discount product discountP)])])))))

(def product  {:name "The Little Prince" :upc 12345 :price 20.25})


(report-product-info product 0.2 0.15)