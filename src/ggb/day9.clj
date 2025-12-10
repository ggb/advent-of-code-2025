(ns ggb.day9
  (:require [clojure.string :as str]))

(def input
  (->> (slurp "data/day9.txt")
       str/split-lines
       (map #(str/split % #","))
       (map #(map parse-long %))
       vec))

(defn area
  [[x1 y1] [x2 y2]]
  (* (- (inc (max x1 x2)) (min x1 x2))
     (- (inc (max y1 y2)) (min y1 y2))))

(defn create-pairs
  [l]
  (for [[i ps] (map-indexed vector l)
        [j qs] (map-indexed vector l)
        :when (< i j)]
    [ps qs]))

;; Part 1
(->> input
     create-pairs
     (map #(apply area %))
     (apply max))

;; Part 2
(defn rect
  [[x1 y1] [x2 y2]]
  [[(min x1 x2) (max x1 x2)] [(min y1 y2) (max y1 y2)]])

(defn check-rect
  [[[x-min x-max] [y-min y-max]] border]
  (not
   (some
    (fn [[[x1 y1] [x2 y2]]]
      (and (< x-min (max x1 x2)) (> x-max (min x1 x2))
           (< y-min (min y1 y2)) (> y-max (max y1 y2))))
    (map vector (cons (last border) border) border))))

(defn max-rect
  [points]
  (fn [acc [a b]]
    (if (and (> (area a b) acc)
             (check-rect (rect a b) points))
      (area a b)
      acc)))

(->> input
     create-pairs
     (reduce (max-rect input) 0)
     time)
