(ns adv2023.day14.sol
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]
   [adv2023.grid :as grid]
   [clojure.math.combinatorics :as combo]
   [clojure.math.numeric-tower :as nu]
   [clojure.core.match :refer [match]]))

(def input
  (->> (slurp "src/adv2023/day14/input.txt")
       (string/split-lines)))

(defn score-right [r]
  (->> r
       (keep-indexed #(if (= \O %2) (inc %1)))
       (reduce +)))

(def tilt-row-right
  (memoize
   (fn [r]
     (->> r (partition-by #{\#}) (mapcat sort)))))

;; part 1
#_(->> (grid/transpose input)
       (map (comp
             score-right
             tilt-row-right
             reverse))
       (reduce +)) ;; => 108614

(def tilt-row-left (memoize (comp reverse tilt-row-right reverse)))

(defn tilt-north [rows]
  (->> rows grid/transpose (map tilt-row-left) grid/transpose))

(defn tilt-west [rows] (map tilt-row-left rows))

(defn tilt-south [rows]
  (->> rows grid/transpose (map tilt-row-right) grid/transpose))

(defn tilt-east [rows] (map tilt-row-right rows))

(def full-cycle (memoize (comp (memoize tilt-east)
                               (memoize tilt-south)
                               (memoize tilt-west)
                               (memoize tilt-north))))

(defn score-grid [rows]
  (->> rows
       grid/transpose
       (map (comp score-right reverse))
       (reduce +)))

(defn find-cycle-length
  ([l] (find-cycle-length (rest l) (rest (rest l))))
  ([[t & ts :as tort]
    [h & hs :as hare]]
   (cond
     (or (empty? tort) (empty? hare)) nil
     (= t h)
     (let [len (- (count tort) (count hare))]
       (if (= (take len tort)
              (take len (drop len tort)))
         len
         (recur (rest tort) (rest (rest hare)))))
     :else (recur (rest tort) (rest (rest hare))))))

#_(find-cycle-length (take 1000 (cycle (concat (shuffle (range 5))
                                               (shuffle (range 5))
                                               (shuffle (range 5))
                                               (shuffle (range 5))))))

(defn part2 [input]
  (let [sample (take 500 (iterate full-cycle input))
        cyc-len (find-cycle-length sample)]
    (->> sample
         (drop (- 1000000000
                  (* cyc-len (int (/ (- 1000000000 100) cyc-len)))))
         first
         score-grid)))

#_(time (part2 input)) ;; => 96447





