(ns leet200_number_of_islands
  [:require graph-util])

(def sample [
             ["1","1","1","1","0"],
             ["1","1","0","1","0"],
             ["1","1","0","0","0"],
             ["0","0","0","0","0"]
             ])

(def g2 [[0 1 0 0]
         [1 1 0 0]
         [0 1 0 1]
         [0 0 1 1]])

(defn is-land [graph v]
  (if-let [val (get-in graph v)]
    (> val 0)))

;; In this matrix representation, the children are any vertically or horizontally
;; adjacent entries that are > 0 and in the bounds of the graph
(defn childrenfn [[y x] graph]
  (let [vs [[(dec y) x] [(inc y) x] [y (dec x)] [y (inc x)]]]
    ;; keep children that are within the bounds of the graph and are > 0
    (filter (partial is-land graph) vs)))

;; Iterate through matrix, and for any node that hasn't yet been visited and is non-zero
;; start a tree search from that node to discover all connected non-zero nodes
;; keepin a count of the disjoint trees that have been discovered
(defn solution [graph]
  (let [maxy (count graph)
        maxx (count (first graph))
        candidate-roots (for [y (range maxy)
                              x (range maxx)
                              :when (is-land graph [y x])]
                          [y x])
        ]
    ;; Reduce over candidate root nodes
    ;; state: n         # number of islands seen
    ;;        marked    set of "land" nodes that have already been explored
    (-> (reduce
          (fn [{:keys [n marked] :as state}
               v]
            (tap> [state v])
            (if (marked v)
              ;; v is part of an existing island, state does not change
              state
              ;; else search for a new island, using v as root
              (let [island-vs (graph-util/bfs graph childrenfn v)]
                {:n      (inc n)
                 :marked (apply conj marked island-vs)}))
            )
          {:n      0
           :marked #{}}
          candidate-roots)
        ;; return just the number of islands seen
        :n)))