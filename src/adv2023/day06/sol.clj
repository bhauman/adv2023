(ns adv2023.day6.sol
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [adv2023.grid :as grid]
   [clojure.set :as set]
   [clojure.edn :as edn]))

(def input
  (->> (slurp "src/adv2023/day06/input.txt")
       (string/split-lines)
       (map #(as-> % x
               (subs x 10)
               (edn/read-string (str "(" x ")"))
             ))
       (grid/transpose)))

(defn race-distance [race-len speed]
  (* (- race-len speed) speed))

(defn winning-distances [[race-len best-dist]]
  (for [speed (range 0 (inc race-len))
        :let [dist (race-distance race-len speed)]
        :when (> dist best-dist)]
    dist))

;; part 1
(->> input
     (map winning-distances)
     (map count)
     (apply *)) ;; => 138915


(def input2
  (->>
   (slurp "src/adv2023/day06/input.txt")
   (string/split-lines)
   (map #(as-> % x
           (subs x 10)
           (string/replace x " " "")
           (edn/read-string x)))))

(def race-len (first input2))
(def best-distance (last input2))

(defn race-dist [speed]
  (race-distance race-len speed))

(defn slope [[x1 y1] [x2 y2]]
  (/ (- y2 y1)
     (- x2 x1)))

(defn find-best-distance-on-line [[x1 y1 :as p1] p2]
  (let [m (slope p1 p2)
        b (- y1 (* m x1))]
    (int (/ (- best-distance b) m)))) ;; floor operation

(defn newtons-method [f start-speed]
  (let [dist-a (f start-speed)
        dist-b (f (inc start-speed))]
    (let [new-start-speed
          (find-best-distance-on-line
           [start-speed dist-a] [(inc start-speed) dist-b])]
      (if (= start-speed new-start-speed)
        start-speed
        (recur f new-start-speed)))))

;; part 2
(let [speed-intercept (newtons-method race-dist 0)]
  (- (dec race-len) (* 2 speed-intercept))) ;; => 27340847





