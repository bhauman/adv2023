(ns adv2023.day21.sol
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]
   [clojure.set :as set]
   [adv2023.grid :as grid]
   #_[clojure.math.combinatorics :as combo]
   #_[clojure.math.numeric-tower :as nu]
   #_[clojure.core.match :refer [match]]
   #_[clojure.data.priority-map :refer [priority-map]]
   #_[medley.core :refer [distinct-by]]))

#_(remove-ns 'adv2022.day21.sol)

(set! *unchecked-math* false) ;; :warn-on-boxed

#_(add-tap (bound-fn* clojure.pprint/pprint))

(def ^:dynamic *input*
  (->> (slurp "src/adv2023/day21/input.txt")
       (string/split-lines)))

(def size (count *input*))

(defn loc-val [pos]
  (get-in *input* (mapv #(mod % size) pos)))

(def start-pos (first
                (for [y (range size)
                      x (range size)
                      :when (= \S (loc-val [y x]))]
                  [y x])))

(def right [0 1])
(def down [1 0])
(def left [0 -1])
(def up   [-1 0])
(def dirs #{right down left up})

(defn next-pos [pos dir] (mapv + pos dir))

(def next-poses
    (memoize
     (fn [pos]
       (for [d dirs
             :let [pos-1 (next-pos pos d)]
             :when (not= \# (loc-val pos-1))]
         pos-1))))

(defn next-positions [positions]
  (->> positions
       (mapcat next-poses)
       (into #{})))

;; part 1
#_(->> (iterate next-positions #{start-pos})
       rest
       (take 64)
       last
       count) ;; => 3776


(defn next-forward-edge [[seen positions]]
  (let [res (into #{}
                  (for [pos   positions
                        pos-1 (next-poses pos)
                        :when (not (seen pos-1))]
                    pos-1))]
    [(set/union seen res) res]))

;; sample edges
(defonce edges-at-n
   (->> (iterate next-forward-edge [#{start-pos} #{start-pos}])
        (map second)
        (take 500)
        vec))

(defn bounds-filter [dir-bounds list-of-edges]
  (->> list-of-edges
       (mapv
        #(set (filter (partial grid/in-bounds? dir-bounds) %)))))

(def right-edges-at-n (bounds-filter [[0 (dec size)] [size Long/MAX_VALUE]] edges-at-n))
(def left-edges-at-n  (bounds-filter [[0 (dec size)] [Long/MIN_VALUE (dec 0)]] edges-at-n))
(def up-edges-at-n    (bounds-filter [[Long/MIN_VALUE (dec 0)] [0 (dec size)]] edges-at-n))
(def down-edges-at-n  (bounds-filter [[size Long/MAX_VALUE] [0 (dec size)]] edges-at-n))

(def right-up-edges-at-n   (bounds-filter [[(- size ) (dec 0)] [size (dec (* 2 size))]] edges-at-n))
(def right-down-edges-at-n (bounds-filter [[size (dec (* 2 size))] [size (dec (* 2 size))]] edges-at-n))
(def left-up-edges-at-n    (bounds-filter [[(- size) (dec 0)] [(- size) (dec 0)]] edges-at-n))
(def left-down-edges-at-n  (bounds-filter [[size (dec (* 2 size))] [(- size) (dec 0)]] edges-at-n))

(defn diagonal-result [ed-at-n n]
  (let [period (dec (int (/ n size)))
        a-n (+ (mod n size) (* 2 size))
        b-n (- a-n size)
        a (count (ed-at-n a-n))
        b (count (ed-at-n b-n))]
    (+ (* period (+ a b)) b)))

(defn cardinal-result [ed-at-n n]
  (count (ed-at-n (+ (mod n size) (* 2 size)))))

(defn edge-count-at-n-help [n]
  (+ (cardinal-result right-edges-at-n n)
     (cardinal-result left-edges-at-n n)
     (cardinal-result up-edges-at-n n)
     (cardinal-result down-edges-at-n n)
     (diagonal-result right-up-edges-at-n n)
     (diagonal-result right-down-edges-at-n n)
     (diagonal-result left-up-edges-at-n n)
     (diagonal-result left-down-edges-at-n n)))

(defn edge-count-at-n [n]
  (if (< n 265)
    (count (edges-at-n n))
    (edge-count-at-n-help n)))

(defn count-all-nodes-at-n [n]
  (->> (range n -1 -2)
       (filter #(>= % 0))
       (map edge-count-at-n)
       (reduce +)))

(assert (= (count-all-nodes-at-n 10000) 89089222))
(assert (= (count-all-nodes-at-n 20000) 356348333))
(assert (= (count-all-nodes-at-n 30000) 801716638))

;; Part 2
#_(def part2 (time (count-all-nodes-at-n 26501365))) ;; => 625587097150084


