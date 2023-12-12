(ns day9.core
  (:require [clojure.string :as string]))

(defn get-input [path]
  (->> path
       slurp
       string/split-lines
       (map #(map read-string (string/split % #"\s+")))))

(def test-input
  (get-input "src/day9/test.txt"))

(def input
  (get-input "src/day9/input.txt"))

(defn derive-line [line]
  (map (fn [[a b]] (- b a))
       (partition 2 1 line)))

(defn predict-line [line]
  (->> line
       (iterate derive-line)
       (take-while #(not (every? zero? %)))
       (map last)
       (reduce +)))

(defn part1 [input]
  (->> input
       (map predict-line)
       (reduce +)))

(defn alternate-subtract [a [index b]]
  (if (even? index) (+ a b) (- a b)))

(defn predict-backwards [line]
  (->> line
       (iterate derive-line)
       (take-while #(not (every? zero? %)))
       (map first)
       (map-indexed vector)
       (reduce alternate-subtract 0)))

(defn part2 [input]
  (->> input
       (map predict-backwards)
       (reduce +)))

(comment
  (last test-input)
  ;; => (10 13 16 21 30 45)

  (predict-line (last test-input))
  ;; => 68

  (part1 test-input)
  ;; => 114

  (part1 input)
  ;; => 1853145119

  (predict-backwards (last test-input))
  ;; => 5

  (part2 test-input)
  ;; => 2

  (part2 input)
  ;; => 923

  0)
