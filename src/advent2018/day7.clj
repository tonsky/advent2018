(ns advent2018.day7
  (:require
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.walk :as walk]
    [advent2018.core :refer :all]))

(def input (slurp "inputs/day7"))
(def lines (str/split input #"\n"))

(defn parse-line [l]
  (let [[_ before after] (re-matches #"Step ([A-Z]) must be finished before step ([A-Z]) can begin\." l)]
    [before after]))

(def parsed-lines (map parse-line lines))

(defn steps []
  (->> (concat (map first parsed-lines)
               (map second parsed-lines))
       set))

(defn initial-reqs
  "{step => #{requirements, ...}, ...}"
  []
  (merge
    (into {}
      (for [s (steps)]
        [s #{}]))
    (reduce
      (fn [m l]
        (let [[before after] l]
          (assoc m after (conj (m after (sorted-set)) before))))
      {}
      parsed-lines)))

(defn try-take
  "Find a step that has empty reqs"
  [reqs]
  (->> reqs (filter #(empty? (second %))) (map first) sort first))

(defn remove-from-reqs
  "Remove step from all reqs of all steps"
  [reqs step]
  (into {}
    (for [[k vals] reqs]
      [k (disj vals step)])))

(defn part1 []
  (loop [reqs (initial-reqs)
         res  ""]
    (if (empty? reqs)
      res
      (let [step  (try-take reqs)
            reqs' (remove-from-reqs reqs step)]
        (recur (dissoc reqs' step) (str res step))))))

#_(time (part1))


(defn duration [step]
  (-> (int (first step))
      (- (int \A))
      (+ 61)))

(defn try-finish
  "Did any of the elfs finished? If yes, return step"
  [q]
  (first
    (for [[step time] q
          :when (zero? time)]
      step)))

(defn work
  "Advance time in queue by 1 sec"
  [q]
  (into (empty q)
    (for [[step time] q]
      [step (dec time)])))

(defn part2 []
  (loop [reqs  (initial-reqs)
         res   ""
         queue (sorted-map) ; { step => time to complete, ... }
         time  0]
    (println time queue res)
    (if (and (empty? reqs)
             (empty? queue))
      time
      (if-some [step (try-finish queue)]
        (recur (remove-from-reqs reqs step) (str res step) (dissoc queue step) time)
        (if (>= (count queue) 5)
          (recur reqs res (work queue) (inc time))
          (if-some [step (try-take reqs)]
            (recur (dissoc reqs step) res (assoc queue step (duration step)) time)
            (recur reqs res (work queue) (inc time))))))))

#_(time (part2))