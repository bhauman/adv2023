(ns adv2023.day5.sol
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [adv2023.grid :as grid]
   [clojure.set :as set]
   [clojure.edn :as edn]))


(defn parse-input [[seeds & maps]]
  {:seeds seeds
   :maps (->> maps
              (partition 2)
              (map (fn [[k rs]]
                     (sort-by second (partition 3 rs))))
              (into []))})

(def input
  (-> (slurp "src/adv2023/day05/input.txt")
      (string/replace "seeds:" "[")
      (string/replace "map:" "[")
      (string/split-lines)
      (->> (map #(if (= % "") "]" %))
           (string/join " "))
      (str "]]")
      (->> (str "[")
           edn/read-string)
      parse-input))

(defn range-limit [start length]
  (+ start (dec length)))

(defn in-range [start length i]
  (<= start i (range-limit start length)))

(defn source->dest-help [[dest source length] i]
  (when (in-range source length i)
    (+ (- i source) dest)))

(defn source->dest [ranges i]
  (or (first (keep #(source->dest-help % i) ranges))
      i))

(defn seed->location [maps seed]
  (if (empty? maps)
    seed
    (recur (rest maps) (source->dest (first maps) seed))))

(defn part1 [{:keys [seeds maps]}]
  (->> seeds
       (map #(seed->location maps %))
       (apply min)))

(part1 input) ;; => 261668924

(defn range-length [start end]
  (inc (- end start)))

(defn range->limits [[start length]]
  [start (range-limit start length)])

(defn limits->range [[start end]]
  [start (range-length start end)])

(defn source-range [[_ source length]]
    [source length])

(def source-range->limits (comp range->limits source-range))

(defn limits-intersect [[start end] [start' end']]
  (when-not (or (< end start')
                (< end' start))
    [(cond
       (<= start start' end) start'
       (<= start' start end') start)
     (cond
       (<= start end' end) end'
       (<= start' end end') end)]))

(defn fill-in-limits [limits]
  (let [lims (sort-by first limits)
        inner-fills (->> limits
                         (mapcat identity)
                         (drop 1)
                         (partition 2)
                         (keep (fn [[a b]]
                                 (when-not (= a (dec b))
                                   [(inc a) (dec b)]))))]
    (concat (when (not (zero? (ffirst lims)))
              [[0 (dec (ffirst lims))]])
            inner-fills
            (when (not= (last (last lims)) Long/MAX_VALUE)
              [[(inc (last (last lims))) Long/MAX_VALUE]]))))

;; fill in all ranges for whole 0 to Long/MAX_VALUE
(defn fill-in-ranges [ranges]
  (let [limits (map (comp range->limits source-range) ranges)
        fills (fill-in-limits limits)
        fill-ranges (->> fills
                         (map limits->range)
                         (map (fn [[a b]] [a a b])))]
    (sort-by second (concat ranges fill-ranges))))

(defn range-intersects [rng rng']
  (some-> (limits-intersect (range->limits rng)
                            (range->limits rng'))
          limits->range))

(defn range->dest-range [start-rng [dest source length :as rng]]
  (when-let [new-range (range-intersect start-rng
                                        (source-range rng))]
    (limits->range
     (mapv #(source->dest-help rng %) (range->limits new-range)))))

(defn ranges->dest-ranges [start-rng ranges]
  (keep #(range->dest-range start-rng %) ranges))

(defn start-ranges->final-ranges [start-rngs [mps & rest-mps]]
  (if (empty? mps)
    start-rngs
    (recur
     (seq (mapcat #(ranges->dest-ranges % mps)
                  start-rngs))
     rest-mps)))

;; part 2
(->> (start-ranges->final-ranges (partition 2 (:seeds input))
                                 (map fill-in-ranges (:maps input)))
     (map first)
     sort
     first) ;; => 24261545







