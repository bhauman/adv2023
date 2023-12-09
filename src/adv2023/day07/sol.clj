(ns adv2023.day07.sol
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [adv2023.grid :as grid]
   [clojure.set :as set]
   [clojure.edn :as edn]))

(def input
  (->> (slurp "src/adv2023/day07/input.txt")
       (string/split-lines)
       (map #(string/split % #"\s"))
       (map (fn [[a b]] [a (parse-long b)]))))

(def card-val  (zipmap [\A \K \Q \J \T] (range 14 9 -1)))

(defn card->number [a] (or (parse-long (str a)) (card-val a)))

(defn hand-canonical-value [[a b & _]] [a (or b 0)])

(defn card-counts [hand]
  (->> hand frequencies vals (sort >) vec))

(defn hand-value [[hand bid]]
  (-> hand
      card-counts
      hand-canonical-value
      (into (map card->number hand ))))

;; part 1
#_(->> input
       (sort-by hand-value)
       (map-indexed (fn [idx [_ bid]]
                      (* (inc idx) bid)))
       (apply +)) ;; => 248812215

(defn joker-card->number [a] (if (= a \J) 1 (card->number a)))

(defn joker-hand-value [[hand bid]]
  (let [j-count (->> hand (filter #{\J}) count)]
    (into (if (= 5 j-count)
            [5 0]
            (-> (remove #{\J} hand)
                card-counts
                (update-in [0] + j-count)
                hand-canonical-value))
          (map joker-card->number hand))))

;; part 2
#_(->> input
       (sort-by joker-hand-value)
       (map-indexed (fn [idx [_ bid]]
                      (* (inc idx) bid)))
       (apply +)) ;; => 250057090




