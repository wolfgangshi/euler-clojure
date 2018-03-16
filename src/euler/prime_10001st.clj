(ns euler.prime-10001st
  (:gen-class))

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
  (empty? (transduce (prime-xf n) conj [] (range 2 (sqrt n))
)))

(defn nth-prime
  [nth]
  (let [xf (comp 
            (filter is-prime?)
            (take nth))]
    (peek (transduce xf conj [] (drop 2 (range))))))

(defn -main
  [& args]
  "By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?"

  (time (nth-prime 10001))
  )
