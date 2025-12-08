(ns ggb.day7
  (:require [clojure.string :as str]))

(def input
  (->> (slurp "data/day7.txt")
       str/split-lines))

(defn at
  [str pos]
  (subs str pos (inc pos)))

(defn check-beam
  [[beam realities] row]
  (if (= "^" (at row beam))
    [1 [[(dec beam) realities] [(inc beam) realities]]]
    [0 [[beam realities]]]))

(defn compact
  [l]
  (for [[k v] (group-by first l)]
    (if (> (count v) 1)
      [k (->> v (map second) (reduce +))]
      (first v))))

(defn check-row
  [beams count row]
  (->> beams
       (map #(check-beam % row))
       (reduce (fn [[acc beams] [c b]]
                 [(+ acc c) (compact (concat b beams))]) [count []])))

(defn trachyon
  [[count beams] rows]
  (if (empty? rows)
    [count beams]
    (let [[c b] (check-row beams count (first rows))]
      (trachyon [c b] (rest rows)))))

(def results (trachyon [0 [[(str/index-of (first input) "S") 1]]] (rest input)))

;; Part 1
(first results)

;; Part 2
(->> results
     second
     (map second)
     (reduce +))
