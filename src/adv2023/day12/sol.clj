(ns adv2023.day12.sol
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]
   [adv2023.grid :as grid]
   [clojure.math.combinatorics :as combo]
   [clojure.math.numeric-tower :as nu]
   [clojure.core.match :refer [match]]))

(def input
  (->> (slurp "src/adv2023/day12/input.txt")
       (string/split-lines)
       (map #(string/split % #"\s"))
       (map (juxt (comp vec first)
                  (comp #(edn/read-string (str "[" % "]"))
                        second)))))

(defn wild-positions [fillable]
  (keep-indexed #(when (= %2 \?) %1) fillable))

(defn fill [fillable positions fil]
  (reduce (fn [ac [a b]] (assoc ac a b)) fillable (map vector positions fil)))

(defn check-sum [well-list]
  (->> well-list
       (partition-by identity)
       (filter #(= \# (first %)))
       (map count)))

(defn poss-solutions [[m chk-sum]]
  (let [w-positions (wild-positions m)
        num-wild (count w-positions)
        num-wells (count (filter #(= \# %1) m))
        num-wells-total (apply + chk-sum)
        num-wells-needed (- num-wells-total num-wells)
        fills (combo/permutations
               (->> (concat (take num-wells-needed (repeat \#)) (repeat \.))
                    (take num-wild)))]
    (map #(fill m w-positions %) fills)))

(defn working-solutions [solutions chk-sum]
  (->> solutions
       (filter #(= (check-sum %) chk-sum))))

(defn solution-count [[pat chk-sum]]
  (count
   (working-solutions (poss-solutions [pat chk-sum])
                     chk-sum)))


#_(working-solutions (poss-solutions [[\? \?] []])
                   [])
;; part 1
#_(->> input
       (map solution-count)
       (apply +)) ;; => 7843

(defn space-needed [chk-sum]
  (+ (count chk-sum) (reduce + chk-sum)))

(defn select [[chk & chks] fillable]
  (let [required-space (space-needed chks)
        num-ways (- (count fillable) required-space chk)]
    [chk (inc num-ways)]))

(defn occupied [chk pos idx]
  (<= pos idx (+ pos (dec chk))))

(defn exact-viable [chk pos fillable]
  (and (<= (+ chk pos) (count fillable))
       (->> (map
             #(condp = %1
                \? true
                \# (occupied chk pos %2)
                \. (not (occupied chk pos %2)))
             fillable
             (range))
            (every? identity))))

(defn viable [chk pos fillable]
  (exact-viable chk pos (take (inc (+ chk pos)) fillable)))

(def sols
  ;; memoizing makes this possible to complete in a timely fashion
  (memoize
   (fn [chks fillable]
    (when (not-empty chks)
      (let [[chk n :as res] (select chks fillable)]
        (if (empty? (rest chks))
          (count (filter #(exact-viable chk % fillable) (range n)))
          (reduce +
                  (map
                   #(sols (rest chks) (drop (+ (inc chk) %) fillable))
                   (filter #(viable chk % fillable) (range n))))))))))

(defn times [n [p chk-sum]]
  [(->> p
        (repeat n)
        (interleave
         (repeat n [\?]))
        (mapcat identity)
        (drop 1)
        vec) 
   (mapcat identity (repeat n chk-sum))])

#_(def part2-result
    (time (reduce + (map #(sols (second %) (first %))
                         (map #(times 5 %) input))))) ;; => 10153896718999









