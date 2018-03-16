(ns euler.smallest-multiple
  (:gen-class))


(def remove-rules
  {20 #{20 4 5 2 10}
   19 #{19}
   18 #{18 3 6 2 9}
   17 #{17}
   16 #{16 4 2 8}
   15 #{15 3 5}
   14 #{14 2 7}
   13 #{13}
   12 #{12 2 6 3 4}
   11 #{11}
   10 #{10 2 5}
   9 #{9 3}
   8 #{8 2 4}
   7 #{7}
   6 #{6 2 3}
   })

(defn divisible-in-range?
  [div-vec n]
  ;; (when (= (mod n 1000) 0) (println n))
  (let [upper-bound (first div-vec)]
    (loop [divs div-vec]
      (if (empty? divs)
        true
        (let [div (first divs)]
          (if (= 0 (mod n div))
            (recur (remove #(contains? (get remove-rules div) %) divs))
            false
            )
          )
        )
      ))
)

(defn smallest-multiple
  [upper-bound]
  (let [xf (comp
            (filter #(divisible-in-range? (range upper-bound 1 -1) %))
            (take 1))]
    (transduce xf + 0 (drop 1 (filter even? (range)))))
 )
(defn -main
 [& args]
 "2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?"
 (println "smallest multiple")
 (time (smallest-multiple 20))
)
