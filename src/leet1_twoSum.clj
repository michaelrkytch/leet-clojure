(ns leet1_twoSum)

;; produce a lazy sequence of the sums of all pairs of vector entries, with their indexes
;; ([[0 0] 6] [[0 1] 5] [[0 2] 7]...
(defn sums [v]
  (for [i (range (count v))
        j (range (count v))]
    [[i j] (+ (v i) (v j))]
    ))

(defn solution [v target]
  (let [mem-sums (memoize sums)]
    (->> v
         mem-sums
         (filter #(= target (second %)))
         ffirst)))
