(ns adv2023.day13.sol
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]
   [adv2023.grid :as grid]
   [clojure.math.combinatorics :as combo]
   [clojure.math.numeric-tower :as nu]
   [clojure.core.match :refer [match]]))

(def input
  (->> (slurp "src/adv2023/day13/input.txt")
       (string/split-lines)
       (partition-by string/blank?)
       (filter #(not= 1 (count %)))))

(defn reflection-at [rows n]
  (let [[a b] (split-at n rows)]
    (every? identity (map = (reverse a) b))))

(defn find-horiz-reflection [rows]
  (first (filter
          #(reflection-at rows %)
          (range 1 (count rows)))))

(defn find-vert-reflection [rows]
  (find-horiz-reflection (grid/transpose rows)))

;; part 1
#_(->> input
     (map (juxt find-vert-reflection find-horiz-reflection))
     (map (fn [[vert horiz]]
            (+ (or vert 0) ((fnil * 0) horiz 100))))
     (reduce +)) ;; => 37113

(defn distance [a b]
  (count (filter identity (map #(not= %1 %2) a b))))

(defn potential-reflection-at [rows n]
  (let [[a b] (split-at n rows)]
    (->> (map distance (reverse a) b)
         (reduce +)
         (= 1))))

(defn find-potential-horiz-reflection [rows]
  (first (filter
          #(potential-reflection-at rows %)
          (range 1 (count rows)))))

(defn find-potential-vert-reflection [rows]
  (find-potential-horiz-reflection (grid/transpose rows)))

#_(->> input
     (map (juxt find-potential-vert-reflection find-potential-horiz-reflection))
     (map (fn [[vert horiz]]
            (+ (or vert 0) ((fnil * 0) horiz 100))))
     (reduce +)) ;; => 30499



