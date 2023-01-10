(ns leet76-minimum-window-substring)

;; General implementation of the "two pointer" pattern
;; Advance a variable length window across the string
;; accept-fn [window] - determine whether the current window meets the criteria
;; compare-fn [a b] - return the best of the 2 candidates (current and previous best)
(defn min-substring-search [accept-fn compare-fn input]
  (let [n (count input)]
    (loop [s 0
           e 1
           best nil
           ]
      (if (= s n)
        ;; we've scanned all substrings, return best
        best
        ;; else
        (let [sub (subs input s e)
              accepted? (and (> (- e s) 0) (accept-fn sub))]
          (if accepted?
            ;; Current window was accepted, try to refine by shrinking window
            (recur (inc s) e (compare-fn sub best))
            ;; else current window was not accepted, increment window end to search for more
            (if (= e n)
              ;; If we're at an unacceptable suffix, then we're done
              best
              ;; else search more substrings
              (recur s (inc e) best))))))))

;; create character histogram
(defn char-histogram [s]
  (reduce #(update %1 %2 (fnil inc 0)) {} s))

(defn solution [s t]
    (let [t-chars (char-histogram t)
          ;; For each of the target characters,
          ;; does window contain at least that many of the same character
          accept? (fn [window]
                    (tap> (str "Considering " window))
                    (let [w-chars (char-histogram window)]
                      (every?
                        (fn [[ch n]]
                          ((fnil >= 0) (w-chars ch) n))
                        t-chars)
                      ))
          ;; Choose the shortest candidate
          compare (fn [a b]
                    (tap> (str "comparing " a ", " b))
                    (cond
                      (nil? a) b
                      (nil? b) a
                      :else
                      (if (< (count a) (count b))
                        a
                        b)))]

      (min-substring-search accept? compare s)))