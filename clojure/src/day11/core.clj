(ns day11.core
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

(defn add-to-grid-set [s [coord tile]]
  (if (= tile ".") s
      (conj s coord)))

(defn construct-galaxy-set [input]
  (reduce add-to-grid-set #{} (mapcat vec input)))

(defn get-input [path]
  (->> path
       slurp
       string/split-lines
       (map-indexed (fn [y line] (map-indexed (fn [x ch] [[x y] ch]) (string/split line #""))))))

(def test-input
  (get-input "src/day11/test.txt"))

(def input
  (get-input "src/day11/input.txt"))

(defn find-empty-lane [input selectfn]
  (let [galaxy-set (construct-galaxy-set input)]
    (set/difference (set (range (count input)))
                    (set (map selectfn galaxy-set)))))

(defn transform-galaxy [[x y] empty-cols empty-rows by]
  (let [empty-cols-before (* (dec by) (count (filter #(<= % x) empty-cols)))
        empty-rows-before (* (dec by) (count (filter #(<= % y) empty-rows)))
        new-x (+ x empty-cols-before)
        new-y (+ y empty-rows-before)]
    [new-x new-y]))

(defn transform-all-galaxies [by input]
  (let [empty-cols (find-empty-lane input first)
        empty-rows (find-empty-lane input second)]
    (map (fn [coord] (transform-galaxy coord empty-cols empty-rows by))
         (construct-galaxy-set input))))

(defn manhatten-distance [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn distance-between-all-pairs [input]
  (map #(manhatten-distance (first %) (second %))
       (combo/combinations input 2)))

(defn part1 [input]
  (->> input
       (transform-all-galaxies 1)
       distance-between-all-pairs
       (reduce +)))

(defn part2 [input by]
  (->> input
       (transform-all-galaxies by)
       distance-between-all-pairs
       (reduce +)))

(comment
  (part1 test-input)
  ;; => 374

  (part1 input)
  ;; => 9805264

  (part2 test-input 10)
  ;; => 1030

  (part2 test-input 100)
  ;; => 8410

  (part2 test-input 1000000)
  ;; => 82000210

  (part2 input 1000000)
  ;; => 779032247216

  0)
