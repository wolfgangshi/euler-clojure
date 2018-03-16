(ns euler.largest-prime-factor
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

(defn xf
  [n]
  (comp
   (filter #(= (mod n %) 0))
   (map #(/ n %))
   (filter odd?)
   (filter is-prime?)
   (take 1)
   )
  )

(defn largest-pf
  [n]
  (first (transduce (xf n) conj []
                    (range 2 n))))
(defn -main
  "The prime factors of 13195 are 5, 7, 13 and 29.

  What is the largest prime factor of the number 600851475143 ?"
  [& args]
  (println "largest-prime-factor")
  (println (largest-pf 13195))
  (time (largest-pf 600851475143))
)
