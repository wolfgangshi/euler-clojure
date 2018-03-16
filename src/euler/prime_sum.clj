(ns euler.prime-sum
  (:gen-class)
  (:require [pyro.printer :as printer])
)

(printer/swap-stacktrace-engine!)

(defn can-divide-by?
  [n div]
  (= 0 (mod n div))
  )

(defn sqrt
  [n]
  (->> n
       (java.lang.Math/sqrt)
       (java.lang.Math/floor)
       (inc)
       (int)
       )
)

(defn prime-xf
  [n]
  (comp
   (filter #(can-divide-by? n %))
   (take 1)
   )
  )

(defn is-prime?
  [n]
  (if (< n 2)
    false
    (empty? (transduce (prime-xf n) conj [] (range 2 (sqrt n))))
    )
)

(defn prime-sum-below
  [n]
  (reduce + 0 (filter is-prime? (range n)))
)

(defn -main
  [& args]
  "The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million."

  (println "prime sum")
  (time (prime-sum-below (nth args 0)))
  )
