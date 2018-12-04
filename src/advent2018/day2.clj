(ns advent2018.day2
  (:require
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.walk :as walk]))


(def input (slurp "inputs/day2"))


(defn appearances
  "'aaabbc' => #{3 2 1}"
  [s]
  (->> (frequencies s)
       (vals)
       (set)))


(defn part1 []
  (let [aps (->> (str/split input #"\n")
              (map appearances))]
    (* (count (filter #(contains? % 2) aps))
       (count (filter #(contains? % 3) aps)))))

#_(part1)


(defn variants
  "'abc' => ['_bc' 'a_c' 'ab_']"
  [s]
  (->> (range 0 (count s))
    (map #(str (subs s 0 %) "_" (subs s (inc %))))))


(defn part2 []
  (->
    (->> (str/split input #"\n")
         (mapcat variants)
         (frequencies) ; {"_bcd" 1, ...}
         (filter (fn [[k v]] (> v 1))) ; [["_bcd" 2]]
         (ffirst))
    (str/replace "_" "")))


#_(part2)