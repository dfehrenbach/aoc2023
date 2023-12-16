(ns day12.core
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

(defn parse-line [line]
  (let [[springs groups] (string/split line #" ")
        groups (mapv read-string (re-seq #"[0-9]+" groups))]
    {:springs springs
     :groups groups}))

(defn get-input [path]
  (->> path
       slurp
       string/split-lines
       (map parse-line)))

(def test-input
  (get-input "src/day12/test.txt"))

(def input
  (get-input "src/day12/input.txt"))

(defn find-arrangements
  ([{:keys [springs groups]}]
   (remove nil? (flatten (find-arrangements (vec (string/split (str springs ".") #"")) groups [] 0))))

  ([springs groups arrangement groupcnt]
   (if (empty? springs)

     (if (and (empty? groups) (zero? groupcnt))
       (apply str arrangement)
       nil)

     (let [character (first springs)
           group (or (first groups) 0)]
       (cond
         (< group groupcnt) nil

           ;; if we've capped a group, start looking for the next gruop
           ;; otherwise, add the character to the arrangement and continue
         (= character ".") (if (pos? groupcnt)
                             (if (not= groupcnt group) nil
                                 (find-arrangements (vec (rest springs)) (rest groups) (conj arrangement character) 0))
                             (find-arrangements (vec (rest springs)) groups (conj arrangement character) 0))

           ;; if we have exceeded the size of the current group, stop, it's invalid
           ;; otherwise, add the character to the arrangement and continue
         (= character "#") (find-arrangements (vec (rest springs)) groups (conj arrangement character) (inc groupcnt))

           ;; try filling it in with a # or a .
         (= character "?") (map #(find-arrangements (vec (concat [%] (vec (rest springs)))) groups arrangement groupcnt)
                                ["." "#"])

         :else nil)))))

(defn part1 [input]
  (->> input
       (map find-arrangements)
       (map count)
       (reduce +)))

(defn expand-input [input]
  (->> input
       (mapv (fn [{:keys [springs groups]}]
               {:springs (apply str (interpose "?" (repeat 5 springs)))
                :groups (vec (apply concat (repeat 5 groups)))}))))

(defn part2 [input]
  (->> input
       expand-input
       (map (fn [line]
              (let [arrangements (find-arrangements line)]
                (prn (count arrangements))
                (count arrangements))))
       (reduce +)))

(comment
  (first test-input)
  ;; => {:springs "???.###", :groups [1 1 3]}

  (find-arrangements (first test-input))
  ;; => ("#.#.###")

  (find-arrangements (second test-input))
  ;; => ("..#...#...###." "..#..#....###." ".#....#...###." ".#...#....###.")

  (part1 test-input)
  ;; => 21

  (part1 input)
  ;; => 7204

  (vec (apply concat (repeat 5 (:groups (first test-input)))))

  (expand-input test-input)

  (count (find-arrangements (nth (part2 test-input) 5)))

;; solutiontime
  (part2 test-input)
  ;; => 525152

  (part2 input)

  0)
