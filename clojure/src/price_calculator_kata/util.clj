(ns price-calculator-kata.util)

(defn round
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn calculate-percentage
  [value percent]
  (round 4 (* value percent)))
