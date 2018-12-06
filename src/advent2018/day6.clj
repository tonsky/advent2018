(ns advent2018.day6
  (:require
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.walk :as walk]
    [advent2018.core :refer :all]))

(def input (slurp "inputs/day6"))
(def lines (str/split input #"\n"))

(def coords (map #(-> % (str/split #", ") (->> (mapv parse-long))) lines))
(def min-x (reduce min (map first coords)))
(def max-x (reduce max (map first coords)))
(def min-y (reduce min (map second coords)))
(def max-y (reduce max (map second coords)))

(defn dist [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x2 x1))
     (Math/abs (- y2 y1))))

(defn idx->letter [i]
  (str (char (+ (int \a) i)))
  i)

(defn closest [[x y]]
  (let [dists    (mapv #(dist % [x y]) coords)
        min-dist (reduce min dists)
        indexes  (->> (map vector dists (range))
                      (filter #(= (first %) min-dist)))]
    (cond
      (> (count indexes) 1) "."
      :else (idx->letter (second (first indexes))))))

(defn rect []
  (doall
    (for [x (range min-x (+ max-x 1))
          y (range min-y (+ max-y 1))]
      [[x y] (closest [x y])])))

(defn boundaries [rect]
  (->> rect
    (filter 
      (fn [[[x y] letter]]
        (or (= x min-x) (= x max-x) (= y min-y) (= y max-y))))
    (map second)
    (set)))

(defn freqs [rect]
  (->> rect
    (map second)
    (frequencies)))

(defn part1 []
  (let [rect       (rect)
        boundaries (boundaries rect)
        freqs      (freqs rect)
        safe       (apply dissoc freqs boundaries)]
    (->> safe
      (map second)
      (reduce max))))

#_(time (part1))


(defn rect2 []
  (doall
    (for [x (range min-x (+ max-x 1))
          y (range min-y (+ max-y 1))]
      [[x y] (reduce + (map #(dist % [x y]) coords))])))

(defn part2 []
  (let [rect2 (rect2)]
    (->> rect2
         (filter (fn [[_ d]] (< d 10000)))
         (count))))

#_(time (part2))
