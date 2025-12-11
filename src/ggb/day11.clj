(ns ggb.day11
  (:require [clojure.string :as str]))

(def input
  (->> (slurp "data/day11.txt")
       str/split-lines
       (map #(str/split % #": "))
       (map (fn [[v ws]] [v (str/split ws #" ")]))
       (into {})))

(defn update-visited
  [must-visit visited node]
  (if (contains? must-visit node)
    (conj visited node)
    visited))

(defn goal-reached?
  [node target visited must-visit]
  (and (= node target) (= visited must-visit)))

(defn dfs
  [graph target must-visit memo node visited]
  (if-let [res (@memo [node visited])]
    res
    (let [updated-visited (update-visited must-visit visited node)
          result (if (goal-reached? node target updated-visited must-visit)
                   1
                   (reduce + (map #(dfs graph target must-visit memo % updated-visited) (graph node))))
          _ (swap! memo assoc [node visited] result)]
      result)))

(defn count-paths
  ([graph start target]
   (count-paths graph start target #{}))
  ([graph start target must-visit]
   (let [memo (atom {})]
     (dfs graph target must-visit memo start #{}))))

;; Part 1
(count-paths input "you" "out")

;; Part 2
(count-paths input "svr" "out" #{"dac" "fft"})
