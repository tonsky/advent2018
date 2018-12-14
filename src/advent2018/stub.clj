(ns advent2018.stub
  (:require
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.walk :as walk]
    [advent2018.core :refer :all]))

(set! *warn-on-reflection* true)

(def input (slurp "inputs/stub"))
(def lines (str/split input #"\n"))

(defn part1 []
  )

#_(time (part1))


(defn part2 []
)

#_(time (part2))

#_(require 'advent2018.stub :reload)