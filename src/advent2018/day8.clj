(ns advent2018.day8
  (:require
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.walk :as walk]
    [advent2018.core :refer :all]))


(def input (slurp "inputs/day8"))
#_(def input "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")
(def numbers (-> input (str/split #" ") (->> (map parse-long))))


(defn read-node [input]
  (let [[children meta & rest] input
        [child-meta child-rest] (reduce
                                  (fn [[meta rest] _]
                                    (let [[meta' rest'] (read-node rest)]
                                      [(+ meta meta') rest']))
                                  [0 rest]
                                  (range 0 children))]
    [(+ child-meta (reduce + 0 (take meta child-rest)))
     (drop meta child-rest)]))


(defn part1 []
  (first (read-node numbers)))

#_(time (part1))


(defn safe-get [coll idx]
  (if (<= 1 idx (count coll))
    (nth coll (dec idx))
    0))


(defn read-node2 [input]
  (let [[children meta & rest] input
        [child-vals child-rest] (reduce
                                   (fn [[vals rest] _]
                                     (let [[val rest'] (read-node2 rest)]
                                       [(conj vals val) rest']))
                                   [[] rest]
                                   (range 0 children))]
    [(if (zero? children)
       (reduce + 0 (take meta child-rest))
       (->> (take meta child-rest)
            (map #(safe-get child-vals %))
            (reduce + 0)))
     (drop meta child-rest)]))


(defn part2 []
  (first (read-node2 numbers)))


#_(time (part2))

