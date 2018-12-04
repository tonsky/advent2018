(ns advent2018.day1
  (:require
    [clojure.string :as str]))

(def input (slurp "inputs/day1"))

(def nums
  (-> input
      (str/split #"\n")
      (->> (map #(Integer/parseInt %)))))


(defn part1 []
  (reduce + 0 nums))

#_(part1)


(defn part2 []
  (loop [sum  0
         seen #{}
         nums (cycle nums)]
    (let [sum' (+ sum (first nums))]
      (if (contains? seen sum')
        sum'
        (recur sum' (conj seen sum') (next nums))))))

#_(part2)