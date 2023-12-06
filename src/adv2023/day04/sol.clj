(ns adv2023.day4.sol
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [adv2023.grid :as grid]
   [clojure.set :as set]))


(def input
  (->> (string/split-lines (slurp "src/adv2023/day04/input.txt"))
       (map #(subs % 10))
       (map #(read-string (str "[#{" (string/replace % "|" "}#{")"}]")))))

(def test-input
  (->> (string/split-lines "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")
       (map #(subs % 8))
       (map #(read-string (str "[#{" (string/replace % "|" "}#{")"}]"))))
  )


(defn score [n]
  (int (Math/pow 2 (dec n))))

(def part1
  (->> test-input
       (map #(score (count (set/intersection (first %) (second %)))))
       #_(reduce +))) ;; => 20107

(defn get-graph [input]
  (let [orig-cards (mapv #(count (set/intersection (first %) (second %)))
                         input)]
    (into {}
          (map-indexed #(let [i (inc %1)]
                          (vector i (range (inc i) (+ (inc i) %2))))
                 orig-cards))))

(def part2-data (get-graph test-input))


(def depth-first-graph
  (memoize (fn [data key]
             (cons key (mapcat #(depth-first-graph data %) (get data key))))))

;; part 2
#_(->> (mapcat #(depth-first-graph part2-data %) (sort > (keys part2-data)))
       count) ;; => 8172507






