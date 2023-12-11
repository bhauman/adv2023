(ns adv2023.day10.sol
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]
   [adv2023.grid :as grid]
   [clojure.core.match :refer [match]]))

(def input
  (->> (slurp "src/adv2023/day10/input.txt")
       (string/split-lines)))

(def dir-vecs {:up [-1 0] :down [1 0] :left [0 -1] :right [0 1]})
(def pipes
  { \F  #{:down :right }
    \7 #{:down :left}
    \- #{:left :right}
    \| #{:up   :down}
    \J #{:up   :left}
    \L #{:up   :right}}
  )

(def input-map
  (into {}
        (for [y (range (count input))
              x (range (count (first input)))]
          [[y x] (get-in input [y x])])))

(def start-point (->> input-map
                      (filter (comp #{\S} second))
                      ffirst))

(defn neighbors [point]
  (->> (get input-map point)
       pipes
       (map dir-vecs)
       (mapv #(mapv + point %))
       not-empty))

(def start-neighbors (some->> input-map
                              keys
                              (keep (juxt identity neighbors))
                              (filter #((set (second %)) start-point))))

(defn point-next [[prev-point point]]
  (some->> (neighbors point)
           (remove #(= prev-point %))
           first
           (vector point)))

(def loop-points
  (->> (iterate point-next [start-point (ffirst start-neighbors)])
       (map second)
       (take-while #(not= start-point %))
       (cons start-point)
       set))

;; part 1
#_(-> loop-points count (/ 2)) ;; => 6640


(defn map-key-for [mp value]
  (ffirst (filter #(= value (val %)) mp)))

(defn direction [point point2]
  (map-key-for dir-vecs (map - point2 point)))

(defn implied-char [point neighs]
  (->> (map (partial direction point) neighs)
       set
       (map-key-for pipes)))

(def part2-input-map
  (-> (reduce-kv
       #(if (loop-points %2) (assoc %1 %2 %3) (assoc %1 %2 \.))
       {} input-map)
      (assoc start-point
             (implied-char start-point (map first start-neighbors)))))

(def new-grid (grid/points->matrix-rows-2d part2-input-map part2-input-map))

(defn simplify-borders [[a b & rs]]
  (cond
    (nil? a) []
    (= \. a) (cons \. (simplify-borders (cons b rs)))
    (= \| a) (cons \| (simplify-borders (cons b rs)))
    (= \- b) (simplify-borders (cons a rs))
    (and a b)
    (match [a b]
           [\F \J] (cons \| (simplify-borders rs))
           [\F \7] (simplify-borders rs)
           [\L \7] (cons \| (simplify-borders rs))
           [\L \J] (simplify-borders rs))))

(declare count-in-loop)
(defn count-out-loop [[a & rs]]
  (cond
    (nil? a) 0
    (= \| a) (count-in-loop rs)
    :else (recur rs)))

(defn count-in-loop [[a & rs]]
  (cond
    (nil? a) 0
    (= \| a) (count-out-loop rs)
    (= \. a) (inc (count-in-loop rs))))

;; part 2
#_ (reduce + (map count-out-loop (map simplify-borders new-grid)))  ; => 411

