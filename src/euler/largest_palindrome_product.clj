(ns euler.largest-palindrome-product
  (:gen-class))


(defn is-palindrome? 
  [n]
  (as-> n $
       (str $)
       (= (clojure.string/reverse $ ) $))
)

(defmacro reversed-range
  [from to]
  `(rseq (vec (range ~from ~to)))
)

(defn gen-num-pairs
  [from to]
  (partition 2 (interleave (reversed-range from to) (repeat (reversed-range from to))))
  )

(defn product
  [[op opv]]
  (let [xf (comp
            (filter #(>= op %))
            (map #(* op %)))]
    (transduce xf conj [] opv)
    )
)

(defn largest-palindrome-product 
  [from to]
  (let [xf (comp
            (mapcat product)
            (filter is-palindrome?)
            )]
    (apply max
     (transduce xf conj [] (gen-num-pairs from to)))
    )
  )

(defn -main
  [& args]
  "A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
Find the largest palindrome made from the product of two 3-digit numbers."
  (println "largest palindrome product")
  (time (largest-palindrome-product 100 1000))
)
