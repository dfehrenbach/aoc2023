
(ns day3.core
  (:require [clojure.string :as string]))

(def max-x 140)
(def max-y 140)

(defn re-fun
  [re s fun]
  (let [matcher (re-matcher re s)]
    (take-while some? (repeatedly #(if (.find matcher) (fun matcher) nil)))))

(defn re-seq-map [re s]
  (into {} (re-fun re s #(vector (.start %) (.group %)))))

(def input
  (->> "src/day3/input.txt"
       slurp
       string/split-lines))

(def input-grid
  (mapv vec input))

(def input-nums
  (map #(re-seq-map #"\d+" %) input))

(defn is-symbol? [c]
  (not (re-find #"[\d.]" (str c))))

(defn coords-around-str [[startX startY] s]
  ;; aaa at x=2, y=2 => [[1 1] [2 1] [3 1] [4 1] [5 1] [1 2] [3 2] [4 2] [5 2] [1 3] [2 3] [3 3] [4 3] [5 3]]
  (let [width (count s)
        xs (range (- startX 1) (+ startX width 1))
        ys (range (- startY 1) (+ startY 2))]
    (for [x xs
          y ys
          :when (and (>= x 0) (< x max-x)
                     (>= y 0) (< y max-y))]
      [x y])))

(defn is-part? [[x y] s]
  (some (fn [[x y]] (is-symbol? (get-in input-grid [y x])))
        (coords-around-str [x y] s)))

(defn filter-parts [y parts]
  (filter (fn [[x s]] (is-part? [x y] s))
          parts))

(map-indexed (fn [y parts]
               (filter-parts y parts))
             input-nums)

(defn part1 []
  (let [parts (map-indexed (fn [y parts]
                             (filter-parts y parts))
                           input-nums)]
    (->> parts
         (mapcat #(map (comp read-string second) %))
         (reduce +))))

(defn part2 [])

(comment
  (part1)
  ;; => 527364

  0)
