(ns advent2018.day11
  (:require
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.walk :as walk]
    [advent2018.core :refer :all]))

(def grid 8561)

(defn hundreds [n]
  (-> n (mod 1000) (quot 100)))

(defn cell-power [x y]
  (-> (+ x 10)
      (* y)
      (+ grid)
      (* (+ x 10))
      (hundreds)
      (- 5)))

(defn square-power [x y size]
  (reduce
    (fn [acc [dx dy]]
      (+ acc (cell-power (+ x dx) (+ y dy))))
    0
    (for [dx (range 0 size)
          dy (range 0 size)]
      [dx dy])))

(defn column-power [x y size]
  (->>
    (for [dy (range 0 size)]
      (cell-power x (+ y dy)))
    (reduce + 0)))

(defn biggest-square
  ([size] (biggest-square size 300))
  ([size field-size]
    (->>
      (for [x (range 1 (+ field-size 2 (- size)))
            y (range 1 (+ field-size 2 (- size)))]
        [(square-power x y size) x y])
      (apply max-key first))))

(defn biggest-square-impure
  ([size] (biggest-square-impure size 300))
  ([size field-size]
    (let [t0          (System/currentTimeMillis)
          *last-power (volatile! nil)
          *best       (volatile! nil)]
      (doseq [y (range 1 (+ field-size 2 (- size)))
              x (range 1 (+ field-size 2 (- size)))]
        (let [power (if (= x 1)
                      (square-power x y size)
                      (+ @*last-power
                         (- (column-power (dec x) y size))
                         (column-power (+ x (dec size)) y size)))]
          (when (or (nil? @*best)
                    (> power (first @*best)))
            (vreset! *best [power x y]))
          (vreset! *last-power power)))
      (cons (- (System/currentTimeMillis) t0) @*best))))

(defn part1 []
  (->> (biggest-square-impure 3)
    (nnext)
    (str/join ",")))

#_(time (part1))

(defn part2 []
  (->>
    (for [size (range 1 301)
          :let [[time power x y] (biggest-square-impure size)
                _ (println (format "size %d, power %d, coords [%d,%d] in %.2f sec" size power x y (double (/ time 1000))))]]
      [power x y size])
    (apply max-key first)
    (next)
    (str/join ",")))

#_(time (part2))