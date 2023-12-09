(ns adv2023.day09.sol
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]))

(def input
  (->> (slurp "src/adv2023/day09/input.txt")
       (string/split-lines)
       (map #(as-> % x
                 (str "[" x "]")
                 (edn/read-string x)))))

(defn diffs [l ]
  (->> l (partition 2 1) (mapv #(apply - (reverse %)))))

(defn next-num [num-seq]
  (let [ds (diffs num-seq)]
    (if (every? zero? ds)
      (last num-seq)
      (+ (last num-seq)
         (next-num ds)))))

;; part 1
#_(reduce + (map next-num input)) ; => 1782868781

(defn prev-num [num-seq]
  (let [ds (diffs num-seq)]
    (if (every? zero? ds)
      (first num-seq)
      (- (first num-seq)
         (prev-num ds)))))

;; part 2
#_ (reduce + (map prev-num input)) ;; => 1057





