(ns adv2023.day15.sol
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]
   [adv2023.grid :as grid]
   [clojure.math.combinatorics :as combo]
   [clojure.math.numeric-tower :as nu]
   [clojure.core.match :refer [match]]))

(def input
  (-> (slurp "src/adv2023/day15/input.txt")
      (string/split-lines)
      first
      (string/split #"\,")))

(defn lens-hash [chs]
  (->> (map int chs)
       (reduce
        #(rem (* (+ %1 %2) 17) 256)
        0)))

;; part 1
#_(->> (map lens-hash input)
       (reduce +)) ;; => 494980

(defn parse-inst [s]
  (if (string/ends-with? s "-")
    [(subs s 0 (dec (count s)))]
    (update (string/split s #"\=") 1 parse-long)))

(defn vec-assoc [l k v]
  {:pre [(vector? l)]
   :post [(vector? %)]}
  (if (some #{k} (map first l))
    (mapv #(if (= k (first %)) [k v] %) l)
    (conj l [k v])))

(defn vec-dissoc [l k]
  {:pre [(vector? l)]
   :post [(vector? %)]}
  (vec (remove #(= k (first %)) l)))

(defn map-inst [m [k v]]
  {:pre [(string? k) (map? m)]
   :post [(map? %)]}
  (update m (lens-hash k)
          (if (nil? v)
            #((fnil vec-dissoc []) % k)
            #((fnil vec-assoc []) % k v))))

(defn score-slot [box slot [k len]]
  {:pre [(every? number? [box slot len])]
   :post [(number? %)]}
  (* (inc box) (inc slot) len))

(defn score-box [[box vec-map]]
  (->> vec-map
       (map-indexed (partial score-slot box))
       (reduce +)))

;; part 2
#_(->> input
       (map parse-inst)
       (reduce map-inst {})
       (map score-box)
       (reduce +)) ; => 247933



