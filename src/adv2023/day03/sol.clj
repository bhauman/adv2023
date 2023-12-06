(ns adv2023.day3.sol
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [adv2023.grid :as grid]))

(def input (string/split-lines (slurp "src/adv2023/day03/input.txt")))

(def test-input
  (string/split-lines
   "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."))

(defn to-grid [lines]
  (->>
   (reduce into
           {}
           (map-indexed
            #(keep-indexed (fn [x c]
                             (when (not= \. c)
                               [[%1 x] c])) %2)
            lines))
   (map (fn [[k v]]
          (let [s (str v)]
            [k (or (parse-long s) s)])))
   (into {})))

#_(to-grid test-input)

(def dirs
  [[-1 0]
   [1 0]
   [0 -1]
   [0 1]
   [-1 -1]
   [1 1]
   [-1 1]
   [1 -1]])

(defn neighbors [location]
  (map #(mapv + location %) dirs))

(defn location-of-nums-next-to [grid location]
  (->> location
       neighbors
       (keep #(find grid %))
       (filter (comp number? val))
       (map key)))

(defn prev-loc [[y x]]
  [y (dec x)])
(defn next-loc [[y x]]
  [y (inc x)])

(defn start-of-num-at [grid loc]
  ;; if previous location is not number then return this location
  (let [prev (prev-loc loc)]
    (if-not (number? (get grid prev))
      loc
      (start-of-num-at grid prev))))

(defn digits-at [grid loc]
  (let [val (grid loc)]
    (if (number? val)
      (str val (digits-at grid (next-loc loc)))
      nil)))

(def num-at (comp parse-long  digits-at))

(defn nums-for-loc [grid loc]
  (->> loc
       (location-of-nums-next-to grid)
       (map #(start-of-num-at grid %))
       (distinct)
       (map #(num-at grid %))))

(defn part-numbers-in-grid [grid]
  (->> (filter #(string? (val %)) grid)
       (map key)
       (mapcat (partial nums-for-loc grid))))

(def part1
  (->> input
       to-grid
       part-numbers-in-grid
       (apply +)))

#_(apply + (part-numbers-in-grid (to-grid test-input)))

(defn gear-ratios-in-grid [grid]
  (->> (filter #(= "*" (val %)) grid)
       (map key)
       (map (partial nums-for-loc grid))
       (filter #(= 2 (count %)))))

(def part2
  (->> input
       to-grid
       gear-ratios-in-grid
       (map #(apply * %))
       (reduce +)))






