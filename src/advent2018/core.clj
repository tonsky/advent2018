(ns advent2018.core)


(defn parse-long [s]
  (when s
    (Long/parseLong s)))

(defn zip [xs ys]
  (map vector xs ys))