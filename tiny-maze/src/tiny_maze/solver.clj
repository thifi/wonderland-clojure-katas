(ns tiny-maze.solver)

(defn bounds [maze]
  (let [x-limit (count (first maze))
        y-limit (count maze)]
    [x-limit y-limit]))

(defn in-bounds? [maze pos]
  (let [[x y] pos
        [x-limit y-limit] (bounds maze)]
    (and (every? #(<= 0 %) pos)
         (< x x-limit)
         (< y y-limit))))

(defn find-element-in-maze [maze element]
  (let [row (first (keep-indexed #(when (some #{element} %2) [%1 %2]) maze))
        col (first (keep-indexed #(when (= element %2) %1) (second row)))]
    [(first row) col]))

(defn find-start-pos [maze]
  (find-element-in-maze maze :S))

(defn find-end-pos [maze]
  (find-element-in-maze maze :E))

(defn hops [pos]
  (let [[x y] pos]
    [
     [(dec x) y]
     [x (inc y)]
     [x (dec y)]
     [(inc x) y]
     ]))

(defn wall? [maze pos]
  (= 1 (get-in maze pos)))

(defn valid-moves [maze pos]
  (let [possible (hops pos)
        in-bounds (filter #(in-bounds? maze %) possible)
        valids (filter #((complement wall?) maze %) in-bounds)]
    valids))

(defn next-moves [maze last-moves]
  (let [valid (valid-moves maze (last last-moves))
        past-filtered (filter (complement (set last-moves)) valid)]
    (map #(conj last-moves %) past-filtered)))

(defn draw-solution [maze solution]
  (loop [s solution
         m maze]
    (if (empty? s) m
        (recur (rest s) (assoc-in m (first s) :x)))))

(defn solve-maze [maze]
  (let [start-pos (find-start-pos maze)
        end-pos (find-end-pos maze)
        paths (tree-seq
               (constantly true)
               (partial next-moves maze)
               [start-pos])
        solutions (filter #(= end-pos (last %)) paths)
        shortest (first (sort-by count solutions))]
    (draw-solution maze shortest)))
