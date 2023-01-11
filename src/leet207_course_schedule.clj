(ns leet207-course-schedule
  [:require graph-util])

;; This is a graph cycle detection problem
;; Courses are nodes, prerequisites is an edge list
;; It is possible to take all courses as long as there are no cycles

;; n - number of courses (named 0 through (n-1))
;; prereqs - a sequence of dependencies, as vertex pairs [source target]
(defn solution [n prereqs]
  (let [sorted (graph-util/topo-sort prereqs)]
    (>= (count sorted) n)))



