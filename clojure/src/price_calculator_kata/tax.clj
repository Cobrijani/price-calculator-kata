(ns price-calculator-kata.tax)

(defn _construct-tax
  [entry]
  (get entry :tax))
(def construct-tax (fnil _construct-tax {:tax 0.2}))
