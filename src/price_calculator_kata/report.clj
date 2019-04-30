(ns price-calculator-kata.report
  (:require [clojure.tools.logging :as log])
  (:require [price-calculator-kata.core :as core]))


(defn report
  [prod]
  (do
    (println (format "Cost = %.2f %s"
                     (core/value (core/initialPrice prod))
                     (core/code (core/initialPrice prod))))
    (println (format "Tax = %.2f %s"
                     (core/value (core/tax prod))
                     (core/code (core/tax prod))))
    (println (if
                 (= 0
                    (core/value
                     (core/discount prod)))
               "No Discounts"
               (format "Discounts = %.2f %s"
                       (core/value
                        (core/discount prod))
                       (core/code
                        (core/discount prod)))))
    (if (empty? (core/expenses prod))
      (println "No Additional costs")
      (doseq [exp (map
                   #(format "%s = %.2f %s"
                            (get % :description)
                            (core/value
                             (get % :cost))
                            (core/code
                             (get % :cost)))
                   (core/expenses prod))]
        (println exp)
             )
      )
    (println (format "TOTAL = %.2f %s"
                     (core/value
                      (core/totalPrice prod))
                     (core/code
                      (core/totalPrice prod))))
    ))
