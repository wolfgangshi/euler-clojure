(ns euler.sum-square-difference
  (:gen-class))

(defn n-multiply-sum-of-rest 
  [n rest]
  (* n (reduce + rest))
)

(defn range-exclude-n
  [n r]
  (remove #(= n %) r)
)

(defn sum-square-difference
  [range-limit]
  (let [r (range 1 (inc range-limit))]
    (reduce (fn [sum n] (+ sum (n-multiply-sum-of-rest n (range-exclude-n n r)))) 0 r)
    )
)

(defn -main
  [& args]
  "The sum of the squares of the first ten natural numbers is,

12 + 22 + ... + 102 = 385
The square of the sum of the first ten natural numbers is,

(1 + 2 + ... + 10)2 = 552 = 3025
Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum."

  (println "sum square difference")
  (time (sum-square-difference 100))
)
