(ns adv2023.day8.sol
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [adv2023.grid :as grid]
   [clojure.set :as set]
   [clojure.edn :as edn]
   [clojure.math.numeric-tower :refer [lcm]]))

(defn parse [[a b & rs]]
  {:directions a
   :locations (as-> rs x
              (string/join x)
              (string/replace x "=" "")
              (str "{" x "}")
              (edn/read-string x))})

(def input
  (->> (slurp "src/adv2023/day08/input.txt")
       (string/split-lines)
       parse))

(defn location-seq [start {:keys [directions locations]}]
  (reductions (fn [loc dir]
                ((if (= dir \L) first second)
                 (get locations loc)))
              start
              (cycle directions)))

;; part 1
#_(->> (location-seq 'AAA input)
       (take-while #(not= 'ZZZ %))
       count) ;; => 16531

;; doesn't work past a certain point but its a quick impl
(defn seive
  ([] (seive 2 (range 2 5000)))
  ([cur-prime next-nums]
   (cons cur-prime
         (lazy-seq
          (let [[next-prime & rest-nums] (remove #(zero? (mod % cur-prime)) next-nums)]
            (seive next-prime rest-nums))))))

(defn is-prime? [x]
  (->> (seive)
       (take-while #(<= % x))
       (filter #(= x %))
       first))

#_(reduce lcm (map (fn [node]
                   (->> input
                        (location-seq node)
                        (take-while #(not= \Z (last (str %))))
                        count))
                 start-nodes))

(defn part2 [{:keys [directions locations]}]
  (let [dir-length (count directions)
        start-nodes (->> (keys locations)
                         (filter  #(= \A (last (str  %)))))
        cycle-lengths (map (fn [node]
                             (->> input
                                  (location-seq node)
                                  (take-while #(not= \Z (last (str %))))
                                  count))
                           start-nodes)
        factors (map #(/ % dir-length) cycle-lengths)]
    (assert (is-prime? dir-length))
    (assert (every? is-prime? factors))
    (assert (apply not= factors))
    (apply * dir-length factors)))

;;(part2 input)
;; => 24035773251517



;; part 2 using lcm very short and sweet impl. Added this
;; after I saw that clojure.math.numeric-tower/lcm was available

(->> (keys (:locations input))
     (filter  #(= \A (last (str  %))))
     (map (fn [node]
            (->> input
                 (location-seq node)
                 (take-while #(not= \Z (last (str %))))
                 count)))
     (reduce lcm))







