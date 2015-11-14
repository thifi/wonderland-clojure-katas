(ns fox-goose-bag-of-corn.puzzle
  (:require [clojure.set :as s]))

(def start-pos [[[:fox :goose :corn :you] [:boat] []]])

(def valid-hops
  {:left [:middle]
   :middle [:left :right]
   :right [:middle]})

(defn index->side [index]
  (case index
    0 :left
    1 :middle
    2 :right))

(defn side->index [index]
  (case index
    :left 0
    :middle 1
    :right 2))

(defn valid-pos? [pos]
  (let [pos-set (map set pos)
        [left middle right] pos-set
        boat-middle? (:boat middle)
        one-item-on-boat? (>= 3 (count middle))
        fox-goose-ok? (empty? (filter #(= #{:fox :goose} %) pos-set))
        goose-corn-ok? (empty? (filter #(= #{:goose :corn} %) pos-set))]
    (and boat-middle?
         one-item-on-boat?
         fox-goose-ok?
         goose-corn-ok?)))

(defn where-are-you? [pos]
  (let [sides-result (map :you (map set pos))
        result-index (keep-indexed #(if ((complement nil?) %2) %1) sides-result)]
    (index->side (first result-index))))

(defn get-side [pos side]
  (nth pos (side->index side)))

;; to-move example: `[:left [:you :goose]]`
;; where example: `:middle`
(defn move-items [pos to-move where]
  (let [[from what] to-move]
    (loop [sides (range 3)
           result []]
      (if (empty? sides)
        (map vec result)
        (let [current (first sides)
              current-key (index->side current)
              current-items (nth pos current)
              transformed-items (condp = current-key
                                  from (remove (set what) current-items)
                                  where (concat current-items what)
                                  current-items)]
          (recur (rest sides) (conj result transformed-items)))))))

(defn get-moves [pos from to]
  (let [[left middle right] pos
        current-side (get-side pos from)
        items (remove #{:you :boat} current-side)
        combos (for [item items] [:you item])
        combos (conj combos [:you])]
    (map #(move-items pos [from %] to) combos)))

(defn moves [past-moves]
  (let [last-pos (last past-moves)
        my-pos (where-are-you? last-pos)
        hops (my-pos valid-hops)
        next-moves (mapcat #(get-moves last-pos my-pos %) hops)
        valid-moves (map vec (filter valid-pos? next-moves))]
    (map #(conj past-moves %) valid-moves)))

(defn is-solution? [moves]
  (let [[left middle right] (last moves)]
    (apply = (map set [right
                       [:you :goose :fox :corn]]))))

(defn river-crossing-plan []
  (let [steps-limit 14]
    (first
     (filter is-solution?
             (last (take steps-limit
                         (iterate #(mapcat moves %) (moves start-pos))))))))
