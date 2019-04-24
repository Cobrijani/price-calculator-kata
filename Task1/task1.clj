(require '[clojure.string :as string])
(defn get-tax
  ;; Retrieve tax for the given product
  ([product] (get-tax product 0.2))
  ([product percentage]  (* (get product :price) (+ 1 percentage))))

(defn round
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))


(defn report-product-info
  ([product] (report-product-info product 0.2))
  ([product percentage]
  (println (string/join "" ["Product price reported as $" (get product :price) " before tax and $" (round 2 (get-tax product percentage)) " after " (* percentage 100) "% tax."]))))

(def product  {:name "The Little Prince" :upc 12345 :price 20.25})

(report-product-info product 0.2)
(report-product-info product 0.21)
