
(ns day5.core
  (:require [clojure.string :as string]))

(defn interpret-seeds [line]
  (let [seeds (re-seq #"\d+" line)]
    (map read-string seeds)))

(defn interpret-range [line]
  (->> line
       (re-seq #"\d+")
       (mapv read-string)
       ((fn [[d s l]]
          {:source-range [s (+ s l -1)] :destination-range [d (+ d l -1)] :shift (- d s)}))))

(defn interpret-map [line]
  (let [[line & maps] (string/split-lines line)
        [_ source-key destination-key] (re-find #"([a-z]+)-to-([a-z]+)" line)
        ranges (map interpret-range maps)]
    {:source (keyword source-key)
     :destination (keyword destination-key)
     :ranges ranges}))

(defn interpret-input [input]
  (let [[seeds maps] (split-at 1 input)]
    {:seeds (interpret-seeds (first seeds))
     :maps (map interpret-map maps)}))

(def test-input
  (->> "src/day5/test.txt"
       slurp
       (#(string/split % #"\r?\n\r?\n"))
       interpret-input))

(def input
  (->> "src/day5/input.txt"
       slurp
       (#(string/split % #"\r?\n\r?\n"))
       interpret-input))

(defn convert-over-ranges [source ranges]
  (let [ranges (filter (fn [m]
                         (let [[start end] (:source-range m)]
                           (<= start source end)))
                       ranges)]
    (if (seq ranges)
      (+ source (:shift (first ranges)))
      source)))

(defn convert-all-sources [sources m]
  (pmap (fn [source]
          (convert-over-ranges source (:ranges m)))
        sources))

(defn convert [{:keys [seeds maps]}]
  (reduce convert-all-sources seeds maps))

(defn part1 [input]
  (->> input
       convert
       (apply min)))

(defn overlap-intersection [[x1 x2] [y1 y2]]
  (when (<= (max x1 y1) (min x2 y2))
    (let [new-start (max x1 y1)
          new-end (min x2 y2)]
      [new-start new-end])))

(defn transform-seed-range [seed-range map-range]
  (let [[start end] seed-range
        [map-start map-end] (:source-range map-range)
        shift (:shift map-range)]
        ;; given a source range and a map range
        ;; determine if the ranges overlap
        ;; if they do, for the overlapping range, create a new range with the :shift applied
        ;; for the non-overlapping range, return that range
        ;; [1 3] [2 4] shift 2 => [1] [4 5] [4]
    (if (<= (max start map-start) (min end map-end))
      (let [overlap (overlap-intersection seed-range (:source-range map-range))
            pre-range (when (< start map-start) [start (dec (first overlap))])
            post-range (when (> end map-end) [(inc (second overlap)) end])]
        {:pre pre-range
         :transformed (mapv #(+ % shift) overlap)
         :post post-range})
      {:not-transformed seed-range})))

(defn transform-seeds-over-map [seed-ranges map-ranges]
  ;; cascade across the map-ranges applying the transformation
  ;; transformed things fill up an empty transformed list
  ;; seed-ranges that reach the end of the map-range transformations are included in the transformed list
  ;; the transformed list of seed ranges is returned when the map-ranges are exhausted
  (loop [seed-ranges seed-ranges
         map-ranges map-ranges
         transformed []]
    (if (seq map-ranges)
      (let [map-range (first map-ranges)
            transformations (map (fn [seed-range]
                                   (transform-seed-range seed-range map-range))
                                 seed-ranges)
            new-transformed (concat transformed (filter seq (map :transformed transformations)))
            new-seed-ranges (concat (filter seq (map :pre transformations))
                                    (filter seq (map :post transformations))
                                    (filter seq (map :not-transformed transformations)))]
        (recur new-seed-ranges (next map-ranges) new-transformed))
      (concat seed-ranges transformed))))

(defn make-seed-ranges [seeds-definition]
  (let [seeds (partition 2 2 seeds-definition)]
    (mapv (fn [[start length]] [start (+ start length)]) seeds)))

(defn part2 [input]
  (let [seed-ranges (make-seed-ranges (input :seeds))
        all-transformed-ranges (reduce transform-seeds-over-map
                                       seed-ranges
                                       (->> input :maps (map :ranges)))]
    (apply min (map first all-transformed-ranges))))

(comment
  (:seeds test-input)
  ;; => (79 14 55 13)

  (first (:maps test-input))
  ;; => {:source :seed,
  ;;     :destination :soil,
  ;;     :ranges
  ;;     ({:source-range [98 100], :destination-range [50 52], :shift -48}
  ;;      {:source-range [50 98], :destination-range [52 100], :shift 2})}

  (convert test-input)
  ;; => (82 43 86 35)

  (part1 test-input)
  ;; => 35

  (part1 input)
  ;; => 331445006

  (part2 test-input)
  ;; => 46

  (part2 input)
  ;; => 6472060

  0)
