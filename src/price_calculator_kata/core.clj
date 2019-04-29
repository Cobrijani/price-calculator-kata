(ns price-calculator-kata.core)


(defprotocol Expense
  (cost [_ _] "return expense amount")
  (description [_] "return expense description"))

(defprotocol Currency
  (code [_] "Return currency ISO-3 code")
  (value [_] "Return currency amount"))


(defprotocol Discount
  (isEligible? [_ _] "checks if product is eligible for a discount")
  (percent [_] "returns discount percent")
  (discountName [_] "returns discount name")
  (afterTax? [_] "whether after or before tax price"))


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


(defn map-price-function
  [entry]
  (case entry
    "initialPrice" initialPrice
    "netPrice" netPrice
    "discountPrice" discountPrice
    "grossPrice" grossPrice))
