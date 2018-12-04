(ns advent2018.day3
  (:require
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.walk :as walk]))

(def input (slurp "inputs/day3"))
(def lines (str/split input #"\n"))

(defn parse-int [s] (Integer/parseInt s))

(defn parse-line [l]
  (let [[_ id l t w h] (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" l)]
    [id
     (parse-int l)
     (parse-int t)
     (+ (parse-int l) (parse-int w))
     (+ (parse-int t) (parse-int h))]))
     
(def rects (mapv parse-line lines))

(defn rect->map [[_ x0 y0 x1 y1]]
  (into {}
    (for [x (range x0 x1)
          y (range y0 y1)]
      [[x y] 1])))

(defn part1 []
  (->> (apply merge-with + (map rect->map rects))
      (filter (fn [[k v]] (> v 1)))
      (count)))

#_(part1)

(defn doesnt-overlap? [[_ x11 y11 x12 y12]
                       [_ x21 y21 x22 y22]]
  (or (>= x21 x12)
      (<= x22 x11)
      (>= y21 y12)
      (<= y22 y11)))

(defn part2 []
  (->>
    (for [r1 rects
          :when (every? #(or (= % r1) (doesnt-overlap? r1 %)) rects)]
      r1)
    ffirst))

#_(part2)