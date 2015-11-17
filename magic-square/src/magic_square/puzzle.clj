(ns magic-square.puzzle
  (:require [clojure.math.combinatorics :as combo]))

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

;;; UTILS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn vectors-sums [vectors]
  (map #(apply + %) vectors))

(defn get-cols [vectors]
  (apply map vector vectors))

(defn get-left-diagonal [vectors]
  (map #(nth %1 %2) vectors (range)))

(defn get-right-diagonal [vectors]
  (map #(nth %1 (- (count %1) %2 1)) vectors (range)))

(defn get-diagonals [vectors]
  [(get-left-diagonal vectors)
   (get-right-diagonal vectors)])

;;; PREDICATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn magical? [square]
  (let [rows-sum (vectors-sums square)
        cols-sum (vectors-sums (get-cols square))
        diag-sum (vectors-sums (get-diagonals square))]
    (apply = (concat rows-sum cols-sum diag-sum))))

;;; SOLUTION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn magic-square [values]
  (let [flat-squares (combo/permutations values)
        squares (map #(partition 3 %) flat-squares)]
    (into [] (map vec (first (filter magical? squares))))))
