(ns ggb.day10
  (:require [clojure.string :as str]))

(def input
  (->> (slurp "data/day10.txt")
       str/split-lines
       (map #(str/split % #" "))))

(def lights
  (->> input
       (map #(->> % first rest drop-last))
       (map (fn [row-chars]
              (let [bools (map #(= % \#) row-chars)]
                (reduce-kv (fn [mask idx v]
                             (if v
                               (bit-set mask idx)
                               mask))
                           0
                           (vec bools)))))))

(def buttons
  (->> input
       (map (comp read-string
                  #(str/replace % #"," " ")
                  #(str "(" % ")")
                  #(apply str %)
                  rest
                  drop-last))
       (map (fn [button-list]
              (map (fn [positions]
                     (reduce bit-set 0 positions))
                   button-list)))))

(defn update-configs
  [depth state start-instructions configs visited]
  (reduce
   (fn [q instr]
     (let [new-state (bit-xor state instr)]
       (if (visited new-state)
         q
         (conj q [(inc depth) new-state]))))
   configs
   start-instructions))

(defn button-presses
  [target-state start-instructions configs]
  (loop [q configs
         visited #{0}]
    (let [[depth state] (peek q)]
      (if (= state target-state) 
        depth
        (let [next-q (update-configs depth state start-instructions (pop q) visited)]
          (recur next-q (conj visited state)))))))

;; Part 1
(->> buttons
     (map #(button-presses %1 %2 (conj clojure.lang.PersistentQueue/EMPTY [0 0])) lights)
     (apply +)
     time)

;; Part 2
;; TODO