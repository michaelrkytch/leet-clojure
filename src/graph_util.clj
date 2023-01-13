(ns graph-util)

;;
;; edge-seq -- representing a graph as a sequence of vertex pairs
;;


(defn edge-map [edge-seq]
  (reduce (fn [m [source target]]
            (update m source #((fnil conj #{}) % target)))
          {} edge-seq))

;; Count indgree of all nodes, return as a map {node : indegree count}
;; TODO: this could be a priority map to optimize searching for roots
(defn indegrees [edge-seq]
  (let [unique-edges (distinct edge-seq)
        ;; Initialize indegree map so that every node has a 0 count
        ;; We do this because we want to include all nodes, including roots
        indegree-map (reduce #(assoc %1 %2 0) {} (map first unique-edges))]
    (reduce
      (fn [m [_ target]]
        (update m target (fnil inc 0)))
      indegree-map
      unique-edges)
    ))

(defn find-root [indegrees]
  (ffirst (filter (fn [[_ n]] (= n 0)) indegrees)))

;; Remove node from indegrees and decrement the indegrees of its neighbors
(defn remove-root [indegrees-map edge-map node]
  (assert (= 0 (indegrees-map node)))
  (let [neighbors (edge-map node)
        indegrees-minus-neighbors (reduce #(update %1 %2 dec) indegrees-map neighbors)]
    (dissoc indegrees-minus-neighbors node)))

;; Return a topologically sorted list of nodes, or nil if there are cycles
(defn topo-sort [edge-seq]
  (let [edge-map (edge-map edge-seq)]
    (loop [indegrees (indegrees edge-seq)
           path []]
      (if-let [next-root (find-root indegrees)]
        (let [new-indegrees (remove-root indegrees edge-map next-root)
              new-path (conj path next-root)]
          (recur new-indegrees new-path))
        ;; else no more roots
        (if (> (count indegrees) 0)
          ;; If there are nodes left to process, then we have a cycle
          nil
          ;; else we have a valid sort
          path)))))


;; Traverse graph from the given root breadth-first.
;; Return the sequence of nodes visited
;; childrenfn [v graph] returns the neihbors of v in graph
;; Graph representation can by anything that supports neighnorsfn
(defn bfs [graph childrenfn root]
  (loop [visitqueue (conj (clojure.lang.PersistentQueue/EMPTY) root)
         visited []]
    (if (empty? visitqueue)
      visited
      ;; else
      (let [v (peek visitqueue)
            neighbors (childrenfn v graph)
            new-visitqueue (apply conj (pop visitqueue) neighbors)]
        (if (some #(= % v) visited)
          ;; if we've already visited v, continue with remaining nodes in queue
          (recur (pop visitqueue) visited)
          ;; else add v to the visited list and add its neighbors to the queue
          (recur new-visitqueue (conj visited v)))))))