(ns advent2018.day13
  (:require
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.walk :as walk]
    [advent2018.core :refer :all])
  (:import
    [clojure.lang ExceptionInfo]))

(set! *warn-on-reflection* true)

(def input (slurp "inputs/day13"))

; (def input (slurp "inputs/day13_example"))
; (def input (slurp "inputs/day13_test"))
; (def input (slurp "inputs/day13_example2"))

(def tracks (-> input
                (str/replace #"[<>^v]" #({"<" "-", ">" "-", "^" "|", "v" "|"} % %))
                (str/split #"\n")
                vec))

(defn track [[x y]]
  (-> tracks (nth y) (nth x)))

(defrecord Cart [pos direction next-turn])

(def carts
  (for [[line y] (zip (str/split input #"\n") (range))
        [char x] (zip line (range))
        :when (#{\< \> \^ \v} char)]
    (Cart. [x y] char :left)))

(defn move-cart [cart]
  (let [{:keys [pos direction next-turn]} cart
        [x y] pos
        intersection? (= \+ (track pos)) 
        [x' y' d'] (if intersection?
                     (case [direction next-turn]
                       [\< :left]     [x       (inc y) \v]
                       [\< :straight] [(dec x) y       \<]
                       [\< :right]    [x       (dec y) \^]
                       [\> :left]     [x       (dec y) \^]
                       [\> :straight] [(inc x) y       \>]
                       [\> :right]    [x       (inc y) \v]
                       [\^ :left]     [(dec x) y       \<]
                       [\^ :straight] [x       (dec y) \^]
                       [\^ :right]    [(inc x) y       \>]
                       [\v :left]     [(inc x) y       \>]
                       [\v :straight] [x       (inc y) \v]
                       [\v :right]    [(dec x) y       \<])
                     (case [direction (track pos)]
                       [\< \-] [(dec x) y \<]
                       [\< \/] [x (inc y) \v]
                       [\< \\] [x (dec y) \^]
                       [\> \-] [(inc x) y \>]
                       [\> \/] [x (dec y) \^]
                       [\> \\] [x (inc y) \v]
                       [\^ \|] [x (dec y) \^]
                       [\^ \/] [(inc x) y \>]
                       [\^ \\] [(dec x) y \<]
                       [\v \|] [x (inc y) \v]
                       [\v \/] [(dec x) y \<]
                       [\v \\] [(inc x) y \>]))]
    (Cart. [x' y'] d' (if intersection? ({:left :straight, :straight :right, :right :left} next-turn) next-turn))))

(defn tick [step carts]
  (loop [coords    (into {}
                     (for [cart carts]
                       [(:pos cart) cart]))
         carts     (sort-by (juxt #(second (:pos %)) #(first (:pos %))) carts)
         new-carts []
         crashed   []]
    (if (empty? carts)
      {:carts   new-carts
       :crashed crashed}
      (let [cart    (first carts)
            coords' (dissoc coords (:pos cart))
            cart'   (move-cart cart)
            pos'    (:pos cart')]
        (if-some [obstacle (coords' pos')]
          (recur coords'
                 (remove #(= % obstacle) (next carts))
                 (remove #(= % obstacle) new-carts)
                 (into crashed [cart' obstacle]))
          (recur (assoc coords' pos' cart')
                 (next carts)
                 (conj new-carts cart')
                 crashed))))))

(defn display [ch [x y]]
  (cond
    (and (= ch \\) (#{\| \+} (track [x (inc y)]))) \╮
    (and (= ch \\) (#{\| \+} (track [x (dec y)]))) \╰
    (and (= ch \/) (#{\| \+} (track [x (inc y)]))) \╭
    (and (= ch \/) (#{\| \+} (track [x (dec y)]))) \╯
    :else ({\- \─
            \| \│
            \+ \┼
            \< "◀︎"
            \> "►"
            \^ "▲"
            \v "▼"
            \space \space} ch)))

(def print? false)
(def print-range [80 120])

(defn print-state [carts crashed]
  (when print?
    (print "\033c") ;; clear screen
    (let [coords (into {} (for [cart (concat carts crashed)]
                            [(:pos cart) cart]))]
      (doseq [[line y] (zip tracks (range))
              :when (<= (first print-range) y (second print-range))
              :let [_ (newline)]
              [track x] (zip line (range))]
        (cond
          (some #(= [x y] (:pos %)) crashed)
          (print (str "\033[0;91m" (display (:direction (coords [x y])) [x y]) "\033[0m"))

          (coords [x y])
          (print (str "\033[0;92m" (display (:direction (coords [x y])) [x y]) "\033[0m"))

          :else
          (print (display track [x y])))))
    (flush)
    (Thread/sleep 250)))

(defn part1 [& [max-step]]
  (loop [step  0
         carts carts]
    (if (> step (or max-step Long/MAX_VALUE))
      :over
      (let [{carts' :carts, crashed :crashed} (tick step carts)]
        (print-state carts' crashed)
        (if (empty? crashed)
          (recur (inc step) carts')
          (let [[{[x y] :pos} & _] crashed]
            (format "Step %s at %d,%d" step x y)))))))

#_(time (part1))


(defn part2 [& [max-step]]
  (loop [step  0
         carts carts]
    (cond
      (> step (or max-step Long/MAX_VALUE))
      :over

      (= 1 (count carts))
      (let [[x y] (:pos (first carts))]
        (format "%d,%d" x y))

      :else
      (let [{carts' :carts
             crashed :crashed} (tick step carts)]
        (print-state carts' crashed)
        (when-not (empty? crashed)
          (println (format "At step %d left: %d, crashed: %s" step (count carts') (mapv #(let [{[x y] :pos, d :direction} %] [x y d]) crashed))))
        (recur (inc step) carts')))))

#_(time (part2))

#_(require 'advent2018.day13 :reload)