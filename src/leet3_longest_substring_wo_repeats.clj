(ns leet3-longest-substring-wo-repeats)

(defn solution [input]
  (let [n (count input)]
    (loop [s 0
           e 1
           best ""
           ]
      (let [sub (subs input s e)
            ;; This would get inefficient for long substrings
            dups? (not= (count sub) (count (set sub)))]
        (if dups?
          (recur (inc s) e best)
          ;; else no dups in substring
          (let [new-best (if (> (count sub) (count best))
                           sub                                ;; current substring is longer, so it's now best
                           best                               ;; else keep previous best
                           )]
            (if (= n e)
              ;; if the window is at the end of the input, return the current best substring
              new-best
              (recur s (inc e) new-best))))))))