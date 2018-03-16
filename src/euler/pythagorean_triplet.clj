(ns euler.pythagorean-triplet
  (:gen-class)
  (:require [pyro.printer :as printer])
)

(printer/swap-stacktrace-engine!)

(defn generate-nn-tuples
  "Generate a lazy sequence of natural number tuples" 
  [max]
  (mapcat
   (fn [x]
     (let [xf (comp 
               (filter #(< % x))
               (map #(vec [% x])))]
       (transduce xf conj [] (range max) )
       ))
   (range 1 max))
)

(defn tuple->c
  [sum a b]
  (- sum a b)
)

(defn pyth-triplet-with-sum?
  [sum]
  (fn
    [[a b]]
    (let [c (tuple->c sum a b)
          a-sqr (* a a)
          b-sqr (* b b)
          c-sqr (* c c)]
      (= c-sqr (+ a-sqr b-sqr))
      ))
)

(defn triple-product
  [sum a b]
  (let [c (tuple->c sum a b)]
    (* a b c)
    )
)

(defn find-pyth-triplet
  [max sum]
  (let [xf (comp
            (filter #((pyth-triplet-with-sum? sum) %))
            (take 1))
        tuple (first (transduce xf conj [] (generate-nn-tuples max)))]
    (apply triple-product (cons sum tuple))
    ))

(defn -main
  [max sum]
  "A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a2 + b2 = c2
For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc."

  (println "Pythagorean triplet: " max sum)
  (take 10 (generate-nn-tuples 10))
  ((pyth-triplet-with-sum? 50) [10 20])
  (find-pyth-triplet max sum)
)
