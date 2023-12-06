(ns adv2023.day2.sol
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(defn parse-game [l]
  (let [[n rest] (-> l (subs 5) (string/split #":"))]
    [(parse-long n)
     (->>
      (read-string
       (str "[["
            (-> rest
                (string/replace ";" "][")
                (str "]]"))))
      (map #(->> %
                 (partition 2)
                 (map (comp vec reverse))
                 (into {}))))]))

(def input (->> (string/split-lines (slurp "src/adv2023/day02/input.txt"))
                (map parse-game)))
  

(def part1-limits
  {'red 12
   'green 13
   'blue 14})

(defn valid? [[n rounds] limits]
  (every?
   #(every? (fn [[k v]]
              (<= v (get limits k)))
            %)
   rounds))

(def part1 (->>
            input
            (filter #(valid? % part1-limits))
            (map first)
            (reduce +))) ;; => 2278

(defn min-cubes [[n rounds]]
      (map (fn [k]
             (apply
              max
              (keep #(% k) rounds)))
           '[red green blue]))

(def part2 (->> input
                (map #(apply * (min-cubes %)))
                (reduce +))) ;; => 67953

