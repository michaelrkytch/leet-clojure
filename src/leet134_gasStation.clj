(ns leet134-gasStation)

;; This solution is O(n^2)
;; A more efficient solution would use the 2-pointer technique:
;; Advance end,adding the net cost of the node until sum gets down to 0
;; While sum <= 0, advance start, subtracting the net cost of each node dropped out
;; of the window,
;; Keep looping these two phases until start gets to the end of the sequence, or until
;; you have a subsequence of size n.

;; Return the net cost of going from node n-1 to node n
(defn net-costs [gas, cost]
  (map - gas cost))

(defn path-cost [i gas cost]
  (let [n (count gas)
        net (vec (net-costs gas cost))
        path-indexes (map #(mod (+ i %) n) (range n))
        cost-seq (map (partial get net) path-indexes)
        ]
    (reductions + cost-seq)))

(defn valid-path [costs]
  (every? (comp not neg?) costs))

(defn find-path [gas cost]
  (let [path-costs (for [i (range (count gas))]
                     [i (path-cost i gas cost)])]
    (ffirst (filter (comp valid-path second) path-costs)))
  )