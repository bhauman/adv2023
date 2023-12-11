(ns adv2023.day11.sol
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]
   [adv2023.grid :as grid]
   [clojure.math.combinatorics :as combo]
   [clojure.core.match :refer [match]]))


(def input
  (->> (slurp "src/adv2023/day11/input.txt")
       (string/split-lines)))

(defn expand [lines]
  (reduce (fn [acc l]
            (cond-> acc
              (every? #{\.} l) (conj l)
              :else (conj l))) [] lines))

(defn expand-universe [mat]
  (->> mat
       expand
       grid/transpose
       expand
       grid/transpose))

(defn galaxies [mat]
  (for [y (range (count mat))
        x (range (count (first mat)))
        :let [ch (get-in mat [y x])]
        :when (= ch \#)]
    [y x]))

(defn distance [[p1 p2]]
  (apply + (map abs (map - p1 p2))))

;; part 2
#_(-> input
      expand-universe
      galaxies
      (combo/combinations 2)
      (->>
       (map distance)
       (reduce +))) ;; => 10289334

(defn universe-y-pos [lines mult y]
  (reduce (fn [acc l]
            (+ acc
               (if (every? #{\.} l) mult 1)))
          0
          (take y lines)))

(defn universe-x-pos [lines mult x]
  (universe-y-pos (grid/transpose lines) mult x))

(defn convert-pos [lines mult [y x]]
  [(universe-y-pos lines mult y)
   (universe-x-pos lines mult x)])

;; part 2
#_(-> input
      galaxies
      (->> (map #(convert-pos input 1000000 %)))
      (combo/combinations 2)
      (->>
       (map distance)
       (reduce +))) ;; => 649862989626


