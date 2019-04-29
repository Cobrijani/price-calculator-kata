(ns price-calculator-kata.expense
  (:require [price-calculator-kata.core
             :refer
             [Expense
              value
              code
              map-price-function]])
  (:require [price-calculator-kata.currency :as currency]))

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
    (fn [x] (currency/->StandardCurrency
             (* percent
                (value (price x)))
             (code (price x))))
    ))
  )


(defn construct-expense
  [entry]
  (case (get entry :type)
    "fixed"
    (static-expense
     (currency/construct-currency
      (get entry :cost))
     (get entry :description))
    "percentual"
    (percentual-expense
     (get entry :percent)
     (map-price-function
      (get entry :of))
     (get entry :description))
    ))
