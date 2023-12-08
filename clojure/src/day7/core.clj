(ns day7.core
  (:require [clojure.string :as string]))

(def face->number {"A" 14 "K" 13 "Q" 12 "J" 11 "T" 10})
(def ranking [:high-card :pair :two-pairs :three-of-a-kind :full-house :four-of-a-kind :five-of-a-kind])

(comment
  (sort by-hand-rank [{:cards [10 6 6 11 6] :ranking :three-of-a-kind}
                      {:cards [10 5 5 11 5] :ranking :three-of-a-kind}]))

(defn interpret-input [line]
  (let [[cards bet] (string/split line #" ")
        cards (mapv #(if (number? (read-string %))
                       (read-string %)
                       (face->number %))
                    (string/split cards #""))]
    {:cards cards :bet (read-string bet)}))

(defn is-five-of-a-kind? [cards]
  ;; without jokers or with them removed, a five-of-a-kind hand represents 1 distinct card (or they're all jokers)
  (or (= 1 (count (dissoc (frequencies cards) 1)))
      (= {1 5} (frequencies cards))))

(defn is-four-of-a-kind? [cards]
  (let [jokers (filter (partial = 1) cards)]
    (if (seq jokers)
      (some #(= 4 (+ % (count jokers))) (vals (dissoc (frequencies cards) 1)))
      (some #(= % 4) (vals (frequencies cards))))))

(defn is-full-house? [cards]
  ;; without jokers or with them removed, a full-house hand represents 2 distinct cards
  (= 2 (count (dissoc (frequencies cards) 1))))

(defn is-three-of-a-kind? [cards]
  (let [jokers (filter (partial = 1) cards)]
    (if (seq jokers)
      (some #(= 3 (+ % (count jokers))) (vals (dissoc (frequencies cards) 1)))
      (some #(= % 3) (vals (frequencies cards))))))

(defn is-two-pairs? [cards]
  ;; without jokers or with them removed, a two-pair hand represents 3 distinct cards
  (= 3 (count (dissoc (frequencies cards) 1))))

(defn is-pair? [cards]
  ;; without jokers or with them removed, a single-pair hand represents 4 distinct cards
  (= 4 (count (dissoc (frequencies cards) 1))))

(defn is-high-card? [cards]
  ;; without jokers or with them removed, a high-card hand represents 5 distinct cards
  (= 5 (count (dissoc (frequencies cards) 1))))

(defn rank-hand [cards]
  (cond
    (is-five-of-a-kind? cards) :five-of-a-kind
    (is-four-of-a-kind? cards) :four-of-a-kind
    (is-full-house? cards) :full-house
    (is-three-of-a-kind? cards) :three-of-a-kind
    (is-two-pairs? cards) :two-pairs
    (is-pair? cards) :pair
    (is-high-card? cards) :high-card
    :else :high-card))

(defn setup-input [input]
  (->> input
       (map interpret-input)
       (map #(assoc % :ranking (rank-hand (:cards %))))))

(def test-input
  (->> "src/day7/test.txt"
       slurp
       string/split-lines))

(def input
  (->> "src/day7/input.txt"
       slurp
       string/split-lines))

(defn by-hand-rank [a b]
  (let [a-rank (.indexOf ranking (:ranking a))
        b-rank (.indexOf ranking (:ranking b))]
    (cond
      (= a-rank b-rank) (compare (:cards a) (:cards b))
      :else (compare a-rank b-rank))))

(defn part1 [input]
  (->> input
       setup-input
       (sort by-hand-rank)
       (map-indexed (fn [i m] (update m :bet (partial * (inc i)))))
       (reduce (fn [acc m] (+ acc (:bet m))) 0)))

(def face->number-part-2 {"A" 14 "K" 13 "Q" 12 "T" 10 "J" 1})
(defn interpret-input-part2 [line]
  (let [[cards bet] (string/split line #" ")
        cards (mapv #(if (number? (read-string %))
                       (read-string %)
                       (face->number-part-2 %))
                    (string/split cards #""))]
    {:cards cards :bet (read-string bet)}))

(defn setup-input-part2 [input]
  (->> input
       (map interpret-input-part2)
       (map #(assoc % :ranking (rank-hand (:cards %))))))

(defn part2 [input]
  (->> input
       setup-input-part2
       (sort by-hand-rank)
       (map-indexed (fn [i m] (update m :bet (partial * (inc i)))))
       (reduce (fn [acc m] (+ acc (:bet m))) 0)))

(comment
  test-input
  ;; => ({:cards [3 2 10 3 13], :bet 765, :ranking :pair}
  ;;     {:cards [10 5 5 11 5], :bet 684, :ranking :three-of-a-kind}
  ;;     {:cards [13 13 6 7 7], :bet 28, :ranking :two-pairs}
  ;;     {:cards [13 10 11 11 10], :bet 220, :ranking :two-pairs}
  ;;     {:cards [12 12 12 11 14], :bet 483, :ranking :three-of-a-kind})

  (sort by-hand-rank test-input)
  ;; => ({:cards [3 2 10 3 13], :bet 765, :ranking :pair}
  ;;     {:cards [13 10 11 11 10], :bet 220, :ranking :two-pairs}
  ;;     {:cards [13 13 6 7 7], :bet 28, :ranking :two-pairs}
  ;;     {:cards [10 5 5 11 5], :bet 684, :ranking :three-of-a-kind}
  ;;     {:cards [12 12 12 11 14], :bet 483, :ranking :three-of-a-kind})

  (part1 test-input)
  ;; => 6440

  (part1 input)
  ;; => 253313241

  (part2 test-input)
  ;; => 5905

  (part2 input)
  ;; => 253362743

  0)
