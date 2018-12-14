(ns advent2018.day12
  (:require
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.walk :as walk]
    [advent2018.core :refer :all])
  (:import
    [java.util List LinkedList ArrayList]))

(set! *warn-on-reflection* true)


(def input (slurp "inputs/day12"))
(def lines (str/split input #"\n"))
(def initial-state [0 (LinkedList. (vec (first lines)))])
(def rules (into {}
             (for [s (next lines)
                   :let [[_ from to] (re-matches #"([.#]+)\s*=>\s*([.#])" s)]]
               [(LinkedList. (vec from)) (first to)])))


(defn next-state [[base-idx ^List s]]
  (loop [i            -2
         base-idx     (- base-idx 2)
         res          (LinkedList.)
         buffer       (int 0)
         iter         (.iterator s)
         surroundings (LinkedList. [\. \. \. \. (.next iter)])]
    (if (> i (+ (count s) 2))
      [base-idx res]
      (let [plant?        (= \# (rules surroundings))
            surroundings' (doto surroundings
                            (.removeFirst)
                            (.addLast (if (.hasNext iter) (.next iter) \.)))]
        (cond
          plant?
          (do
            (dotimes [_ buffer] (.add res \.))
            (.add res \#)
            (recur (inc i) base-idx res (int 0) iter surroundings'))

          (empty? res)
          (recur (inc i) (inc base-idx) res buffer iter surroundings')

          :else
          (recur (inc i) base-idx res (inc buffer) iter surroundings'))))))


(defn results [[base-idx s]]
  (reduce
    (fn [sum [ch idx]]
      (if (= \# ch)
        (+ sum idx)
        sum))
    0
    (map vector (vec s) (range base-idx Long/MAX_VALUE))))


(defn part1 [gen]
  (results (nth (iterate next-state2 initial-state) gen)))


#_(time (part1 20))


(defn part2 [gen]
  (let [[base-idx s] (nth (iterate next-state2 initial-state) 1000)]
    (results [(- gen  (- 1000 base-idx)) s])))


#_(time (part2 50000000000))

#_(require 'advent2018.day12 :reload)


