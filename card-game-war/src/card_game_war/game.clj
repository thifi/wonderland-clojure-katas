(ns card-game-war.game)

;; feel free to use these cards or use your own data structure
(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(def cards
  (for [suit suits
        rank ranks]
    [suit rank]))

(defn rank-index [card]
  (.indexOf ranks (second card)))

(defn suit-index [card]
  (.indexOf suits (first card)))

(defn higher-suit [player1-card player2-card]
  (let [suit1 (suit-index player1-card)
        suit2 (suit-index player2-card)]
    (cond
      (> suit1 suit2) :player1
      (< suit1 suit2) :player2)))

(defn higher-player [player1-card player2-card]
  (let [rank1 (rank-index player1-card)
        rank2 (rank-index player2-card)]
    (cond
      (> rank1 rank2) :player1
      (< rank1 rank2) :player2
      (= rank1 rank2) (higher-suit player1-card player2-card))))

(defn play-round [player1-card player2-card]
  (let [cards [player1-card player2-card]
        high-player (apply higher-player cards)]
    (case high-player
      :player1 [cards nil]
      :player2 [nil cards])))

(defn play-game [player1-cards player2-cards]
  (loop [p1 player1-cards
         p2 player2-cards]
    (let [empty-map (map empty? [p1 p2])]
      (if (some true? empty-map)
        (case empty-map
          [false true] :player1
          [true false] :player2)
        (let [round-result (play-round (first p1) (first p2))
              rest-decks [(rest p1) (rest p2)]
              [next-p1 next-p2] (map
                                 #(when-not (nil? %2) (conj %1 %2))
                                 rest-decks round-result)]
          (recur next-p1 next-p2))))))

(defn deal-cards []
  (let [half (/ (count cards) 2)
        shuffled (shuffle cards)]
    (split-at half shuffled)))
