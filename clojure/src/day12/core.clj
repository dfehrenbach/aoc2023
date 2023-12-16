(ns day12.core
  (:require [clojure.string :as string]))

(defn parse-line [line]
  (let [[springs groups] (string/split line #" ")
        groups (map read-string (re-seq #"[0-9]+" groups))]
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

(def find-arrangements
  (memoize (fn [springs groups groupcnt]
             (if (empty? springs)
               (if (and (empty? groups) (zero? groupcnt))
                 1 0)
               (let [character (first springs)
                     group (or (first groups) 0)]
                 (+
                  (if (some #{"#" "?"} [character])
                    (find-arrangements (rest springs) groups (inc groupcnt))
                    0)
                  (if (some #{"." "?"} [character])
                    (if (pos? groupcnt)
                      (if (not= groupcnt group) 0
                          (find-arrangements (rest springs) (rest groups) 0))
                      (find-arrangements (rest springs) groups 0))
                    0)))))))

(defn part1 [input]
  (->> input
       (pmap (fn [{:keys [springs groups]}]
               (find-arrangements (string/split (str springs ".") #"") groups 0)))
       (reduce +)))

(defn expand-input [input]
  (->> input
       (mapv (fn [{:keys [springs groups]}]
               {:springs (string/join "?" (repeat 5 springs))
                :groups (vec (flatten (repeat 5 groups)))}))))

(defn part2 [input]
  (->> input
       expand-input
       (pmap (fn [{:keys [springs groups]}]
               (find-arrangements (string/split (str springs ".") #"") groups 0)))
       (reduce +)))

(comment
  (first test-input)
  ;; => {:springs "???.###", :groups [1 1 3]}

  (find-arrangements (string/split (str (:springs (first test-input)) ".") #"") (:groups (first test-input)) 0)
  ;; => 1

  (find-arrangements (string/split (str (:springs (second test-input)) ".") #"") (:groups (second test-input)) 0)
  ;; => 4

  (part1 test-input)
  ;; => 21

  (part1 input)
  ;; => 7204

  (part2 test-input)
  ;; => 525152

  (part2 input)
  ;; => 1672318386674

  0)
