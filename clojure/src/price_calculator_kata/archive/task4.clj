;; There is a special discount assigned to a product with specified (configurable) UPC.
;; This discount only applies to a product with UPC value equal to the value defined by the discount.
;; If both universal and UPC-based discounts are applicable, they both apply to original product price and then sum up.
;; When two discounts are applied, only the total discounted amount is printed (requirement REPORT-DISCOUNT).

;; Definition of done:
;; Sample product: Title = “The Little Prince”, UPC=12345, price=$20.25.

;; Case #1:
;; Tax = 20%, universal discount = 15%, UPC-discount = 7% for UPC=12345
;; Tax amount = $20.25 * 20% = $4.05, discount = $20.25 * 15% = $3.04, UPC discount = $1.42
;; Program prints price $19.84
;; Program reports total discount amount $4.46

;; Case #2:
;; Tax = 21%, universal discount = 15%, UPC-discount = 7 for UPC = 789
;; Tax amount = $20.25 * 21% = $4.25, discount = $20.25 * 15% = $3.04
;; Program prints price $21.46
;; Program reports discount amount $3.04
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
  (discount [_] [_ _] "Return discount value for the product")
  (grossPrice [_] "return price with tax")
  (discountPrice [_] [_ _] "return price with discounts")
  (totalPrice [_] [_ _] "return total price with tax and discounts"))

(defprotocol Discount
  (isEligible? [_ _] "checks if product is eligible for a discount")
  (percent [_] "returns discount percent")
  (discountName [_] "returns discount name"))

(defn calculate
  "calculate discount for a product"
  [_ product]
(if (isEligible? _ product)
 (calculate-percentage (initialPrice product) (percent _))
 0
 ))

(defrecord UniversalDiscount [discountP]
  Discount
  (isEligible? [_ product] true)
  (percent [_] discountP)
  (discountName [_] "Universal Discount"))

(defrecord UPCDiscount [upcCode discountP]
  Discount
  (isEligible? [_ product] (= upcCode (upc product)))
  (percent [_] discountP)
  (discountName [_] "UPC Discount"))



(defrecord StandardProduct [productName upcVal price taxP]
  Product
  (initialPrice [_] price)
  (productName [_] productName)
  (upc [_] upcVal)
  (tax [_] (calculate-tax (initialPrice _) taxP))
  (discount [_] 0)
  (discount [_ discounts]
  (if (empty? discounts)
   (discount _)
   (let [[f & other] discounts]
    (+ (calculate (eval f) _) (discount _ other))
   )
  ))
  (grossPrice [_] (+ (initialPrice _) (tax _)))
  (discountPrice [_] (initialPrice _))
  (discountPrice [_ discounts] (- (initialPrice _) (discount _ discounts)))
  (totalPrice [_] (+ (discountPrice _) (tax _)))
  (totalPrice [_ discounts] (+ (discountPrice _ discounts) (tax _))))




(def disc (->UniversalDiscount 0.15))
(def upcDisc (->UPCDiscount 12345 0.15))



;; Case #1:
;; Tax = 20%, universal discount = 15%, UPC-discount = 7% for UPC=12345
;; Tax amount = $20.25 * 20% = $4.05, discount = $20.25 * 15% = $3.04, UPC discount = $1.42
;; Program prints price $19.84
;; Program reports total discount amount $4.46
(println "Case #1")
(def prod (->StandardProduct "The Little Prince" 12345 20.25 0.2))
(def upcDisc1 (->UPCDiscount 12345 0.07))
(println (format "Price %s$" (totalPrice prod `(disc upcDisc1))))
(println (format "Discount %s$" (discount prod `(disc upcDisc1))))


;; Case #2:
;; Tax = 21%, universal discount = 15%, UPC-discount = 7 for UPC = 789
;; Tax amount = $20.25 * 21% = $4.25, discount = $20.25 * 15% = $3.04
;; Program prints price $21.46
;; Program reports discount amount $3.04
(println "Case #2")
(def prod2 (->StandardProduct "The Little Prince" 12345 20.25 0.21))
(def upcDisc2 (->UPCDiscount 789 0.07))
(println (format "Price %s$" (totalPrice prod2 `(disc upcDisc2))))
(println (format "Discount %s$" (discount prod2 `(disc upcDisc2))))

