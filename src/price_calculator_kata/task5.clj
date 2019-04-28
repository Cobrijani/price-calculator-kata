;; 5. PRECEDENCE
;; By this point, tax had precedence over any discounts. That means that tax was always applied to full price of the product, not to the discounted price.
;; Customer is happy to announce that some discounts can legally be applied before tax. That has the consequence that the tax amount would be lower.
;; Extend the solution so that discounts can either be applied before tax calculation, or after tax calculation.

;; Definition of done:
;; Sample product: Title = “The Little Prince”, UPC=12345, price=$20.25.
;; Tax = 20%, universal discount (after tax) = 15%, UPC-discount (before tax) = 7% for UPC=12345

;; UPC discount amount = $1.42, remaining price = $20.25 - $1.42 = $18.83
;; Tax amount = $18.83 * 20% = $3.77, universal discount = $18.83 * 15% = $2.82
;; Final price = $20.25 - $1.42 + $3.77 - $2.82 = $19.78

;; Program prints price $19.78
;; Program reports total discount amount $4.24
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


(defprotocol Product
  (initialPrice [_] "Return initial price")
  (productName [_] "Return product name")
  (upc [_] "Return product upc")
  (tax [_] "Return the tax amount")
  (netPrice [_] "Return price before tax")
  (discount [_] "Return discount value for the product")
  (grossPrice [_] "return price with tax")
  (discountPrice [_]"return price with discounts")
  (totalPrice [_] "return total price with tax and discounts"))

(defprotocol Discount
  (isEligible? [_ _] "checks if product is eligible for a discount")
  (percent [_] "returns discount percent")
  (discountName [_] "returns discount name")
  (afterTax? [_] "whether after or before tax price"))

(defn calculate
  "calculate discount for a product"
[_ product]
  (if (isEligible? _ product)
    (if (afterTax? _)
      (calculate-percentage (netPrice product) (percent _))
      (calculate-percentage (initialPrice product) (percent _))
    )
    0
  )

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



(defrecord StandardProduct [productName upcVal price taxP discounts]
  Product
  (initialPrice [_] price)
  (productName [_] productName)
  (upc [_] upcVal)
  (tax [_] (calculate-tax (netPrice _) taxP))
  (netPrice [_] (- (initialPrice _) (reduce + (map #(calculate %  _) (filter #(not(afterTax? %)) discounts)))))
  (discount [_]
  (if (empty? discounts)
   0
   (reduce + (map #(calculate % _) discounts))))
  (grossPrice [_] (+ (netPrice _) (tax _)))
  (discountPrice [_] (- (initialPrice _) (discount _)))
  (totalPrice [_] (+ (discountPrice _) (tax _))))



;; Definition of done:
;; Sample product: Title = “The Little Prince”, UPC=12345, price=$20.25.
;; Tax = 20%, universal discount (after tax) = 15%, UPC-discount (before tax) = 7% for UPC=12345
(def unAfterTax (->UniversalDiscount 0.15 true))
(def upcBefore (->UPCDiscount 12345 0.07 false))
(def prod (->StandardProduct "The Little Prince" 12345 20.25 0.2 [unAfterTax upcBefore]))


;; UPC discount amount = $1.42, remaining price = $20.25 - $1.42 = $18.83
;; Tax amount = $18.83 * 20% = $3.77, universal discount = $18.83 * 15% = $2.82
;; Final price = $20.25 - $1.42 + $3.77 - $2.82 = $19.78


(println (format "Price %.2f$" (totalPrice prod)))
(println (format "Discount %s$" (discount prod)))

;; Program prints price $19.78
;; Program reports total discount amount $4.24