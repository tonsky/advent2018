(ns advent2018.day10
  (:require
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.walk :as walk]
    [advent2018.core :refer :all]))

(def input (slurp "inputs/day10"))
(def lines (str/split input #"\n"))

(defn parse-line [l]
  (let [[x y vx vy] (re-seq #"-?\d+" l)]
    [(parse-long x) (parse-long y) (parse-long vx) (parse-long vy)]))

(def points (mapv parse-line lines))

(defn next-point [[x y vx vy]]
  [(+ x vx) (+ y vy) vx vy])

(defn next-step [points]
  (mapv next-point points))

(defn get-x [[x _ _ _]] x)
(defn get-y [[_ y _ _]] y)

(defn bounding-box [points]
  [(reduce min (map get-x points))
   (reduce min (map get-y points))
   (reduce max (map get-x points))
   (reduce max (map get-y points))])

(defn area [[x1 y1 x2 y2]]
  (* (Math/abs (- x2 x1)) (Math/abs (- y2 y1))))

(defn draw-points [points]
  (let [[x1 y1 x2 y2] (bounding-box points)
        coords        (into #{}
                        (for [[x y _ _] points]
                          [x y]))]
    (str/join "\n"
      (for [y (range y1 (inc y2))]
        (str/join
          (for [x (range x1 (inc x2))]
            (if (coords [x y]) "█" " ")))))))

(defn parts []
  (loop [points   points
         second   0
         min-area (area (bounding-box points))]
    (let [points' (next-step points)
          bb'     (bounding-box points')
          area'   (area bb')]
      (if (<= area' min-area)
        (recur points' (inc second) area')
        (println (format "second %d, area %d, bounding box %s\n%s" second min-area (bounding-box points) (draw-points points)))))))

#_(time (parts))