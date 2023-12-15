(ns day10.core
  (:require [clojure.string :as string]))

(def pipe->coord-link
  ;; north is negative y, south is positive y
  ;; west is negative x, east is positive x
  {"|" [[0 -1] [0 1]] ;; vertical
   "-" [[-1 0] [1 0]] ;; horizontal
   "L" [[0 -1] [1 0]] ;; north and east
   "J" [[0 -1] [-1 0]] ;; north and west
   "7" [[0 1] [-1 0]] ;; south and west
   "F" [[0 1] [1 0]] ;; south and east
   "S" [[-1 0] [1 0] [0 -1] [0 1]] ;; any direction
   "." nil ;; goes nowehere
   })

(defn add-to-grid-map [m [coord tile]]
  (if (= tile \.) m
      (assoc m coord
             (mapv (fn [[dx dy]] [(+ dx (first coord)) (+ dy (second coord))])
                   (pipe->coord-link tile)))))

(defn construct-grid [input]
  (reduce add-to-grid-map {} input))

(defn get-input [path]
  (->> path
       slurp
       string/split-lines
       (map-indexed (fn [y line] (map-indexed (fn [x ch] [[x y] ch]) (string/split line #""))))
       (mapcat vec)
       construct-grid))

(def test1-input
  (get-input "src/day10/test1.txt"))

(def test2-input
  (get-input "src/day10/test2.txt"))

(def input
  (get-input "src/day10/input.txt"))

(defn get-start [grid-map]
  (->> grid-map
       (filter #(= (count (second %)) 4))
       (map first)
       first))

(defn find-loop [grid-map]
  (let [start (get-start grid-map)
        possible-directions (filter #(seq (grid-map %)) (grid-map start))]
    (loop [current (second possible-directions)
           last-at start
           visited [start]
           steps 0]
      (if (= start current) {:visited visited :steps steps}
          (if (nil? current) nil
              (let [next-option (first (remove #{last-at} (grid-map current)))]
                (recur next-option current (conj visited current) (inc steps))))))))

(defn part1 [input]
  (->> input
       find-loop
       :steps
       inc
       (#(/ % 2))))

(defn shoelace [[x1 y1] [x2 y2]]
  (+ (* x1 y2) (- (* x2 y1))))

(defn shoelace [[x1 y1] [x2 y2]]
  (* (+ y1 y2) (- x1 x2)))

(defn apply-shoelace [coords]
  (loop [sum 0 coords coords]
    (if (nil? (second coords)) (abs (/ sum 2))
        (recur (+ sum (shoelace (first coords) (second coords))) (rest coords)))))

(defn inner-points-picks-theorem [area boundary-points]
  (+ area 1 (- (/ boundary-points 2))))

(defn part2 [input]
  (let [path (:visited (find-loop input))
        area (apply-shoelace (conj path (first path)))]
    (inner-points-picks-theorem area (count path))))

(comment
  (filter seq (find-loop test1-input))
  ;; => ({:visited [[1 1] [2 1] [3 1] [3 2] [3 3] [2 3] [1 3] [1 2]], :steps 7}
  ;;     {:visited [[1 1] [1 2] [1 3] [2 3] [3 3] [3 2] [3 1] [2 1]], :steps 7})

  (filter seq (find-loop test2-input))
  ;; => ({:visited [[0 2] [1 2] [1 1] [2 1] [2 0] [3 0] [3 1] [3 2] [4 2] [4 3] [3 3] [2 3] [1 3] [1 4] [0 4] [0 3]],
  ;;      :steps 15}
  ;;     {:visited [[0 2] [0 3] [0 4] [1 4] [1 3] [2 3] [3 3] [4 3] [4 2] [3 2] [3 1] [3 0] [2 0] [2 1] [1 1] [1 2]],
  ;;      :steps 15})

  (filter (fn [d] (seq (test1-input d)))
          (test1-input (get-start test1-input)))
  ;; => ([2 1] [1 2])

  (part1 test1-input)
  ;; => 4

  (part1 test2-input)
  ;; => 8

  (part1 input)
  ;; => 6860

  (part2 test1-input)
  ;; => 1

  (part2 input)
  ;; => 343

  0)
