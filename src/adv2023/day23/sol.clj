(ns adv2023.day23.sol
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]
   [clojure.set :as set]
   [adv2023.grid :as grid]))

#_(remove-ns 'adv2022.day23.sol)

#_(add-tap (bound-fn* clojure.pprint/pprint))

(def ^:dynamic *input*
  (->> (slurp "src/adv2023/day23/input.txt")
       (string/split-lines)))

(def size (count *input*))

(defn loc-val [pos]
  (get-in *input* (mapv #(mod % size) pos)))

(def start-pos [0 1])
(def end-pos [(dec size) (- size 2)])

(assert (= \. (loc-val end-pos)))

(def right [0 1])
(def down [1 0])
(def left [0 -1])
(def up   [-1 0])

(def dir-ch {\> right \< left \^ up \v down})

(defn next-pos [pos dir] (mapv + pos dir))

(defn next-poses-icey [pos]
  (for [d (if-let [only-dir (dir-ch (loc-val pos))]
            [only-dir]
            (vals dir-sym))
        :let [pos-1 (next-pos pos d)]
        :when (not= \# (loc-val pos-1))]
    pos-1))

(defn stack-path-lengths [next-fn start-pos]
  (loop [[[seen pos] & rs :as stack] (list [#{} start-pos])
         result []]
    (cond
      (nil? seen) result
      (= end-pos pos) (recur rs (conj result (count seen)))
      :else
      (recur (into rs (for [pos' (remove seen (next-fn pos))]
                        [(conj seen pos) pos']))
             result))))

;; part 1
#_(reduce max (stack-path-lengths next-poses-icey start-pos)) ;; =>  2414

(def next-poses
  (memoize
   (fn [pos]
     (for [d (vals dir-sym)
           :let [pos-1 (next-pos pos d)]
           :when (not= \# (loc-val pos-1))]
       pos-1))))

(def segment-positions-to-filter
  (memoize
   (fn [segment-list]
     (->> segment-list
          (remove #(<= (count %) 4))
          (mapcat (comp butlast butlast rest rest))))))

;; get the map as a graph of weighted segments
(defn stack-path-segments [next-fn start-pos]
  (loop [[[seen segment] & rs :as stack] (list [#{} (list start-pos)])
         segment-list []]
    (let [pos (first segment)]
      (cond
        (nil? seen) segment-list
        (= end-pos pos) (recur rs (conj segment-list segment))
        :else
        (if-let [next (->> (next-fn pos)
                           (remove (into seen (segment-positions-to-filter segment-list)))
                           not-empty)]
          (let [next-seen (conj seen pos)]
            (if (= 1 (count next))
              (recur (conj rs [next-seen (cons (first next) segment)])
                     segment-list)
              (recur (into rs (map #(vector next-seen (list % pos)) next))
                     (conj segment-list segment))))
          (recur rs segment-list))))))

(def path-segments
    (->> (stack-path-segments next-poses start-pos)
         (map (fn [path] [#{(last path) (first path)} path]))))

(def segment-graph
  (->> path-segments
       (reduce (fn [acc [nodes rs]]
                 (let [[node-1 node-2] (seq nodes)
                       len (dec (count rs))]
                   (-> acc
                       (update node-1 conj [node-2 len])
                       (update node-2 conj [node-1 len]))))
               {})))

;; do depth first search over the graph for longest path
(defn longest-path-length [graph seen pos]
  (if (= pos end-pos)
    0
    (some->> (get graph pos)
             (remove (comp seen first))
             not-empty
             (keep #(when-let [res (longest-path-length
                                    graph
                                    (conj seen pos) (first %))]
                      (+ res (second %))))
             not-empty
             (reduce max 0))))

;; part 2   90 seconds

#_(time (longest-path-length segment-graph #{} start-pos))

;; => 6598
