(ns advent2018.day5
  (:require
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.walk :as walk]
    [advent2018.core :refer :all]))

(def input (slurp "inputs/day5"))

(defn invert [ch]
  (if (Character/isUpperCase ch)
    (Character/toLowerCase ch)
    (Character/toUpperCase ch)))

(defn destroy-one [s i]
  (loop [i i]
    (if (>= i (dec (count s)))
      [s]
      (if (= (nth s i)
             (invert (nth s (inc i))))
        [(str (subs s 0 i) (subs s (+ i 2))) i]
        (recur (inc i))))))

(defn part1 [input]
  (loop [s input
         i 0]
    (let [[s' i'] (destroy-one s i)]
      (if (nil? i')
        (count s)
        (recur s' (max 0 (dec i')))))))

#_(time (part1 input))

(defn part2 []
  (reduce
    (fn [m ch]
      (let [CH (Character/toUpperCase ch)
            input'  (->> input
                      (remove #{ch CH})
                      (str/join))
            reduced (part1 input')]
        (println ch CH "=>" reduced)
        (min m reduced)))
    Long/MAX_VALUE
    "qwertyuiopasdfghjklzxcvbnm"))

#_(time (part2))