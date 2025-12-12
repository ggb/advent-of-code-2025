(ns ggb.day12
  (:require [clojure.string :as str]))

(def input
  (-> (slurp "data/day12.txt")
      (str/split #"\n\n")
      last
      str/split-lines))

(defn parse-helper
  [row]
  (map parse-long (re-seq #"[0-9]+" row)))

(defn filter-helper
  [[width height & rest]]
  (< (* (apply + rest) 7) (* width height)))

;; Part 1
(->> input 
     (map parse-helper)
     (filter filter-helper)
     count)