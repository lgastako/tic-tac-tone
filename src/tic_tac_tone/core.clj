(ns tic-tac-tone.core
  (:use [launchtone.core :only [make-app set-board! on-button]]
        [launchtone.cron :only [after]]
        [launchtone.utils :only [debug set-level!]]))

;;(set-level! :debug)

(def empty-spots
  [:e :e :e
   :e :e :e
   :e :e :e])

(defn which-spot
  [row col]
  ;; TODO: Math this up.
  (let [mapping {[0 0] 0 [0 1] 0 [1 0] 0 [1 1] 0
                 [0 3] 1 [0 4] 1 [1 3] 1 [1 4] 1
                 [0 6] 2 [0 7] 2 [1 6] 2 [1 7] 2
                 [3 0] 3 [3 1] 3 [4 0] 3 [4 1] 3
                 [3 3] 4 [3 4] 4 [4 3] 4 [4 4] 4
                 [3 6] 5 [3 7] 5 [4 6] 5 [4 7] 5
                 [6 0] 6 [6 1] 6 [7 0] 6 [7 1] 6
                 [6 3] 7 [6 4] 7 [7 3] 7 [7 4] 7
                 [6 6] 8 [6 7] 8 [7 6] 8 [7 7] 8}]
    (mapping [row col])))

(defn spots->board
  [spots]
  (vec (for [r (range 8)]
         (vec (for [c (range 8)]
                (let [spot (which-spot r c)]
                  (if spot (spots spot) :y)))))))

(def empty-board (spots->board empty-spots))

(defn valid-move?
  [app spot]
  (let [spots (@app :spots)]
    (if spot
      (= (spots spot) :e)
      false)))

(defn other-player
  [player]
  (if (= player :r) :g :r))

(defn move
  [spot]
  (fn [app]
    (let [spots (app :spots)
          player (app :turn)
          new-spots (assoc spots spot player)]
      (assoc app
        :spots new-spots
        :turn (other-player player)
        :board (spots->board new-spots)))))

(defn make-move!
  [app spot]
  (swap! app (move spot)))

(defn spots-full?
  [app]
  (let [spots (@app :spots)]
    (= 0 (count (filter #(= % :e) (flatten spots))))))

(defn find-win
  [player spots]
  (let [winning-combos [[0 1 2]
                        [3 4 5]
                        [6 7 8]
                        [0 3 6]
                        [1 4 7]
                        [2 5 8]
                        [0 4 8]
                        [2 4 6]]]
    (let [ts (map (fn [line]
                    (map (fn [spot]
                           (= player (spots spot)))
                         line))
                  winning-combos)
          index (.indexOf ts [true true true])]
      (when (not= index -1)
        (winning-combos index)))))

(defn winner
  [spots]
  (if (find-win :r spots)
    :r
    (when (find-win :g spots)
      :g)))

(defn game-over?
  [app]
  (let [spots (@app :spots)
        winner (winner spots)]
    (or (spots-full? app)
        winner)))

(defn reset-board!
  [app]
  (set-board! app empty-board))

(defn start-game!
  [app]
  (swap! app #(assoc %
                :turn :r
                :spots empty-spots))
  (reset-board! app))

(defn rcs-of
  [positions]
  ;; TODO: Elegantize
  (let [mapping {0 [[0 0] [0 1]
                    [1 0] [1 1]]
                 1 [[0 3] [0 4]
                    [1 3] [1 4]]
                 2 [[0 6] [0 7]
                    [1 6] [1 7]]
                 3 [[3 0] [3 1]
                    [4 0] [4 1]]
                 4 [[3 3] [3 4]
                    [4 3] [4 4]]
                 5 [[3 6] [3 7]
                    [4 6] [4 7]]
                 6 [[6 0] [6 1]
                    [7 0] [7 1]]
                 7 [[6 3] [6 4]
                    [7 3] [7 4]]
                 8 [[6 6] [6 7]
                    [7 6] [7 7]]}]
    (apply concat (map #(mapping %) positions))))

(defn rec-assoc
  [board rcs player]
  (if (not (pos? (count rcs)))
    board
    (let [[r c] (first rcs)]
      (recur (assoc-in board [r c] player)
             (rest rcs)
             player))))

(defn set-positions!
  [app positions player]
  (let [board (@app :board)]
    (let [rcs (rcs-of positions)]
      (let [new-board (rec-assoc board rcs player)]
        (set-board! app new-board)))))

(defn flash-winner!
  [app player positions]
  (let [t-dur 500
        off-dur 250]
                                        (set-positions! app positions :e)
    (after app off-dur                 #(set-positions! app positions player))
    (after app t-dur                   #(set-positions! app positions :e))
    (after app (+ off-dur t-dur)       #(set-positions! app positions player))
    (after app (* 2 t-dur)             #(set-positions! app positions :e))
    (after app (+ off-dur (* 2 t-dur)) #(set-positions! app positions player))))

(defn flash-draw!
  [app]
  (let [board (@app :board)]
    (flash-winner! app :y (range 9))
    (after app 1500 #(set-board! app board))))

(defn show-winner!
  [app]
  (if-let [win-positions (find-win :r (@app :spots))]
    (flash-winner! app :r win-positions)
    (if-let [win-positions (find-win :g (@app :spots))]
      (flash-winner! app :g win-positions)
      (flash-draw! app))))

(defn handle-move
  [app]
  (fn [row col event-type m]
    (if (game-over? app)
      (start-game! app)
      (let [player (@app :turn)]
        (let [spot (which-spot row col)]
          (when (valid-move? app spot)
            (make-move! app spot)
            (when (game-over? app)
              (show-winner! app))))))))

(defn -main []
  (let [app (make-app)]
    (on-button app (handle-move app) ::key-press)
    (start-game! app)))
