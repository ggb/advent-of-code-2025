(ns ggb.day6
  (:require [clojure.string :as str]))

(def input
  (->> (slurp "data/day6.txt")
       str/split-lines))

;; Part 1
(->> input
     (map str/trim)
     (map #(str/split % #"\s+"))
     (apply mapv vector)
     (map reverse)
     (map #(apply (resolve (symbol (first %))) (map parse-long (rest %))))
     (reduce +))

;; Part 2
(def indizes
  (->> input
       last
       (map-indexed (fn [i e] (when (not= e \space) i)))
       (filter some?)
       vec))

(defn parts
  [size s]
  (map #(subs s %1 (dec %2)) (conj indizes size) (rest (conj indizes size))))

(defn handle-column
  [col]
  (->> col
       (map seq)
       (apply mapv vector)
       (map #(apply str %))
       (map str/trim)
       (map parse-long)))

(->> input
     drop-last
     (map (partial parts (inc (-> input last count))))
     (apply mapv vector)
     (map handle-column)
     (map #(apply (resolve (symbol %1)) %2) (str/split (last input) #"\s+"))
     (reduce +))
