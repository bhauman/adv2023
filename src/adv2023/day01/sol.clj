(ns adv2023.day1.sol
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def input (string/split-lines (slurp "src/adv2023/day01/input.txt")))

(defn to-number [digits converter]
  (->> digits
      ((juxt first last))
      (map converter)
      ((juxt (comp #(* 10 %) first) last))
      (apply +)))

(def part1
  (->> input
       (map #(re-seq #"\d" %))
       (map #(to-number % parse-long))
       (reduce +)));; => 56506

;; Part 2

(def str->digit
  (into
   {"one" 1
    "two" 2
    "three" 3
    "four" 4
    "five" 5
    "six" 6
    "seven" 7
    "eight" 8
    "nine" 9}
   (map (juxt str identity) (range 1 10))))

(defn first-to-occur [strs s]
  (->> (keep #(when-let [idx (string/index-of s %)]
                [idx %]) strs)
       (sort-by first)
       first
       second))

(defn last-to-occur [strs s]
  (string/reverse (first-to-occur (map string/reverse strs) (string/reverse s))))

(defn to-number [[a b]]
  (+ (* 10 (str->digit a))
     (str->digit b)))

(def part2
  (->> input
       (map (juxt (partial first-to-occur (keys str->digit))
                  (partial last-to-occur (keys str->digit))))
       (map to-number)
       (reduce +)))
