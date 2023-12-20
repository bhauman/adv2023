(ns adv2023.day19.sol
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]))

#_(remove-ns 'adv2022.day19.sol)

(defn parse-wflw [line]
  (-> line
      (string/replace #"(\w)(\<|\>)(\d+)\:(\w+)" "(:$2 $1 $3 $4)")
      (string/replace #"\{" "(")
      (string/replace #"\}" ")")
      (#(edn/read-string (str "(" % ")")))
      ((juxt first (comp (partial map #(if (symbol? %) [%] %)) second)))))

(def in
  (->> (slurp "src/adv2023/day19/input.txt")
       (string/split-lines)
       (split-with (complement string/blank?))
       ((juxt first (comp rest second)))))

(def workflows (into {} (map parse-wflw (first in))))
(def parts (->> in second
                (map #(string/replace % #"=" " "))
                (map edn/read-string)))

(defn dispatch [[cond? rating value res] part]
  (condp = cond?
    :> (when (> (get part rating) value) res)
    :< (when (< (get part rating) value) res)
    cond?))

(defn exec-wf [part wf]
  (reduce (fn [acc inst]
            (when-let [res (dispatch inst part)]
              (reduced res))) nil wf))

(defn resolve-part [part]
  (loop [wf 'in]
    (let [next (exec-wf part (get workflows wf))]
      (assert (symbol? next))
      (cond
        (= 'A next) part
        (= 'R next) nil
        :else (recur next)))))

;; part 1
(->> (keep resolve-part parts)
     (mapcat vals)
     (reduce +)) ;; => 333263


(defn max-or-min [min-max-map [cond? rating value]]
  (let [[cur-min cur-max] (get min-max-map rating [1 4001])]
    (cond-> min-max-map
      (and (= :> cond?) (> value cur-min)) (assoc rating [(inc value) cur-max])
      (and (= :< cond?) (< value cur-max)) (assoc rating [cur-min value]))))

(defn invert-cond [[cond? rating value]]
  (condp = cond?
    :< [:> rating (dec value)]
    :> [:< rating (inc value)]))

(defn number-of-ways [min-max-map]
  (->> (vals min-max-map)
       (map #(apply - (reverse %)))
       (#(concat % (repeat 4000)))
       (take 4)
       (reduce *)))

(defn constraints [so-far [[cond? rating value res :as inst] & insts]]
  (cond
    (= 'A cond?) (number-of-ways (reduce max-or-min {} so-far))
    (= 'R cond?) nil
    (#{:> :<} cond?)
    (keep identity
          [(some->> (get workflows res [[res]])
                    (constraints (conj so-far [cond? rating value])))
           (some->> insts
                    (constraints (conj so-far (invert-cond [cond? rating value]))))])
    :else
    (constraints so-far (get workflows cond?))))

;; part 2
#_(->> (constraints [] (get workflows 'in))
       flatten
       (reduce +)) ;; => 130745440937650


