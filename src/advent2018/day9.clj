(ns advent2018.day9
  (:require
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.walk :as walk]
    [advent2018.core :refer :all])
  (:import
    [java.util List LinkedList ArrayList]))

; 9, 25 => 32
; 10 players; last marble is worth 1618 points: high score is 8317
; 13 players; last marble is worth 7999 points: high score is 146373
; 17 players; last marble is worth 1104 points: high score is 2764
; 21 players; last marble is worth 6111 points: high score is 54718
; 30 players; last marble is worth 5807 points: high score is 37305
; 410 players; last marble is worth 72059 points

(defn list-create [value]
  (let [*n (volatile! {:value value})]
    (vswap! *n assoc :next *n :prev *n)
    *n))

(defn list-prev [*n] (:prev @*n))
(defn list-next [*n] (:next @*n))
(defn list-peek [*n] (:value @*n))
(defn list-pop [*n]
  (let [*before (list-prev *n)
        *after  (list-next *n)]
    (vswap! *before assoc :next *after)
    (vswap! *after assoc :prev *before)
    *after))

(defn list-add [*n value]
  (let [*after (list-next *n)
        *n'    (volatile! {:value value :prev *n :next *after})]
    (vswap! *n assoc :next *n')
    (vswap! *after assoc :prev *n')
    *n'))

(defn list-vals [*n]
  (->> (iterate list-next *n)
       (next)
       (take-while #(not (identical? *n %)))
       (cons *n)
       (mapv list-peek)))

(defn part1 [players marbles]
  (loop [marble  1
         *circle (list-create 0)
         scores  {}]
    (if (> marble marbles)
      (reduce max (vals scores))
      (if (zero? (mod marble 23))
        (let [*circle' (nth (iterate list-prev *circle) 7)
              removed-marble (list-peek *circle')]
          (recur (inc marble)
                 (list-pop *circle')
                 (update scores (mod marble players) #(+ (or % 0) marble removed-marble))))
        (recur (inc marble)
               (-> *circle list-next (list-add marble))
               scores)))))

#_(time (part1 9 25)) ; => 32
#_(time (part1 10 1618)) ; => 8317
#_(time (part1 410 72059)) ; => 429287
#_(time (part1 410 720590)) ; => 37171915
#_(time (part1 410 7205900)) ; => 3624387659

(defn part2 []
  (part1 410 7205900))

#_(time (part2))