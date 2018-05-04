(ns astar-algorithm-clojure.core)

(def graph {:A {:C 14, :B 12},
            :B {:C 9, :D 38},
            :C {:E 7, :D 24},
            :D {:G 9},
            :E {:D 13, :G 29, :F 9}
            :F {},
            :G {}})

(def H {:A 30,
        :B 26,
        :C 21,
        :D 7,
        :E 22,
        :F 36
        :G 0})

(defn state-children
  [state]
  (keys (state graph)))

(defn children-values
  [state]
  (vals (state graph)))

(defn h
  [n]
  (n H))

(defn g
  [cost current-cost]
  (+ cost current-cost))

(defn evaluation
  [current-cost state cost]
  (+ (g cost current-cost) (h state)))

(defn get-costs
  [state current-cost]
  (let [evaluation (partial evaluation current-cost)]
    (map evaluation (state-children state) (children-values state))))

(defn get-next-state
  [state costs]
  (nth (state-children state) (.indexOf costs (apply min costs))))

(defn get-real-cost
  [current-cost state next-state]
  (+ current-cost ((state graph) next-state)))

(defn goal-test
  [state]
  (= state :G))

(defn search-A*
  ([state] (search-A* state 0 []))
  ([state current-cost path]
   (if (goal-test state)
     (let [path (conj path state)]
       (println path)
       (println current-cost))
     (if (not (empty? (state graph)))
       (let [costs (get-costs state current-cost)
             next-state (get-next-state state costs)]
         (recur next-state (get-real-cost current-cost state next-state) (conj path state)))))))

;(defn search-A*
;  ([state] (search-A* state 0))
;  ([state current-cost]
;
;   (if (goal-test state)
;     (do
;       (println current-cost)
;       (println "Chegou"))
;     (if (not (empty? (state graph)))
;       (do
;         (def costs (get-costs state current-cost))
;         (def next-state (get-next-state state costs))
;         (println state)
;         (search-A* next-state (get-real-cost current-cost state next-state)))))))

(defn main
  []
  (search-A* :A))