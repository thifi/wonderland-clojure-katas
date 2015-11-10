(ns wonderland-number.finder)

(defn set-str [n]
  (set (str n)))

(defn exp-10 [n]
  (apply * (repeat n 10)))

(defn wonderland-number? [n]
  (->> (range 1 7)
       (map * (repeat n))
       (map set-str)
       (apply =)))

(defn wonderland-number []
  (let [r (range (exp-10 5) (exp-10 6))]
    (first (filter wonderland-number? r))))
