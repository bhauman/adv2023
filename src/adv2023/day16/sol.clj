(ns adv2023.day16.sol
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]
   [adv2023.grid :as grid]
   [clojure.math.combinatorics :as combo]
   [clojure.math.numeric-tower :as nu]
   [clojure.core.match :refer [match]]))

#_(remove-ns 'adv2022.day16.sol)

#_(add-tap (bound-fn* clojure.pprint/pprint))

(def ^:dynamic *input*
  (-> (slurp "src/adv2023/day16/input.txt")
      (string/split-lines)))

(def size (count *input*))
(def get-loc (partial get-in *input*))

(def right [0 1])
(def down [1 0])
(def left [0 -1])
(def up   [-1 0])

(defn next-pos [dir pos]
  (mapv + dir pos))

(def dispatch-map
  {\/ {down  [left]
       up    [right]
       right [up]
       left  [down]}
   \\ {down  [right]
       up    [left]
       left  [up]
       right [down]}
   \| {left [up down]
       right [up down]}
   \- {down [left right]
       up [left right]}
   })

(defn in-bounds? [[y x :as p]]
  (and (<= 0 y (dec size))
       (<= 0 x (dec size))))

(defn next-dir-pos [[dir pos]]
  (->> (get-in dispatch-map [(get-loc pos) dir] [dir])
       (map (juxt identity #(next-pos % pos)))
       (filter (comp in-bounds? second))))

(def next-dir-pos-mem (memoize next-dir-pos))

(defn step [list-of-dir-pos]
  (mapcat next-dir-pos-mem list-of-dir-pos))

(defn stepper [seen-atom list-of-dir-pos]
  (let [res (remove @seen-atom (step list-of-dir-pos))]
    (swap! seen-atom into res)
    res))

(defn solve-from [dir-pos]
  (->> [dir-pos]
       (iterate (partial stepper (atom #{})))
       (take-while not-empty)
       (map #(map second %))
       (reduce into #{})
       count))

;; part 1
#_(solve-from [right [0 0]])
;; => 7728

(defn all-edge-dir-pos []
  (concat
   ;; top
   (map #(vector down %)  (map #(vector 0 %) (range size)))
   ;; bottom
   (map #(vector up %)    (map #(vector (dec size) %) (range size)))
   ;; left
   (map #(vector right %) (map #(vector % 0) (range size)))
   ;; right  
   (map #(vector left %)  (map #(vector % (dec size)) (range size)))))

;; part 2
#_(time (reduce max (map solve-from (all-edge-dir-pos))))
;; => 8061



