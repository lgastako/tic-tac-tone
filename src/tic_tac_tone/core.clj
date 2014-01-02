(ns tic-tac-tone.core
  (:use [launchtone.core :only [make-app set-board! on-button]]
        [launchtone.utils :only [debug set-level! enumerate]]))

(set-level! :debug)

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

(assert (= empty-board
           [[:e :e :y :e :e :y :e :e]
            [:e :e :y :e :e :y :e :e]
            [:y :y :y :y :y :y :y :y]
            [:e :e :y :e :e :y :e :e]
            [:e :e :y :e :e :y :e :e]
            [:y :y :y :y :y :y :y :y]
            [:e :e :y :e :e :y :e :e]
            [:e :e :y :e :e :y :e :e]]))

(assert (= 2 (which-spot 0 6)))

(defn valid-move?
  [app spot]
  (debug "valid-move?")
  (debug "spots " (@app :spots))
  (debug "spot " spot)
  (let [spots (@app :spots)]
    (if spot
      (= (spots spot) :e)
      false)))

(assert (valid-move? (atom {:spots [:e :e :e
                                    :e :e :e
                                    :e :e :e]}) 1))

(defn other-player
  [player]
  (if (= player :r) :g :r))

(defn move
  [spot]
  (debug "outer move")
  (fn [app]
    (debug "inner move")
    (let [spots (app :spots)
          player (app :turn)
          new-spots (assoc spots spot player)]
      (debug "spots " spots)
      (debug "player " player)
      (debug "new-spots " new-spots)
      (assoc app
        :spots new-spots
        :turn (other-player player)
        :board (spots->board new-spots)))))

(defn make-move!
  [app spot]
  (debug "make-move! " spot)
  (swap! app (move spot)))

(defn board-full?
  [app]
  (let [spots (@app :spots)]
    (= 0 (count (filter #(= % :e) (flatten spots))))))

(defn find-win
  [player spots]
  (let [winning-combos [[[0 0] [0 1] [0 2]]
                        [[1 0] [1 1] [1 2]]
                        [[2 0] [2 1] [2 2]]
                        [[0 0] [1 0] [2 0]]
                        [[0 1] [1 1] [2 1]]
                        [[0 2] [1 2] [2 2]]
                        [[0 0] [1 1] [2 2]]
                        [[0 2] [1 1] [2 0]]]]
    (let [tfs (map (fn [line] (map (fn [[r c]] (= player ((spots r) c))) line)) winning-combos)]
      (pos? (count  (filter #(= % [true true true]) tfs))))))

(defn winner
  [spots]
  (if (find-win :r spots)
    :r
    (when (find-win :g spots)
      :g)))

(defn game-over?
  [app]
  (debug "game-over?")
  (let [spots (@app :spots)
        winner (winner spots)]
    (or (board-full? app)
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

(defn handle-move
  [app]
  (debug "handle-move")
  (fn [row col event-type m]
    (debug "handle-move details " row " " col)
    (if (game-over? app)
      (do
        (debug "game over")
        (start-game! app))
      (let [player (@app :turn)]
        (debug "player " player)
        (let [spot (which-spot row col)]
          (debug "spot " spot)
          (when (valid-move? app spot)
            (debug "valid move!")
            (make-move! app spot)))))))

(defn -main []
  (let [app (make-app)]
    (on-button app (handle-move app) ::key-press)
    (start-game! app)))
