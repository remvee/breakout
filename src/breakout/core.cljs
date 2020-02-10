(ns breakout.core
  (:require [breakout.math :as math]
            [goog.events :as events]
            [goog.events.EventType :as event-type]
            [reagent.core :as r]))

;; Number of blocks in the game.
(def block-columns 10)
(def block-rows 10)

;; Minimum angle (in degrees) the ball bounces of the paddle.
(def angle-constraint 15)

;; Ball size in percentage of field size.
(def ball-size 2)
(def ball-on-paddle-y (- 1 (/ (* 1.5 ball-size) 100)))

;; Block margins, gap size between blocks.
(def block-margin .002)

;; Start of game state.
(def new-world {:paddle {:x .5, :width .1}
                :ball   {:x .525, :y ball-on-paddle-y, :dx 0, :dy 0, :speed .02, :on-paddle true}
                :blocks [[nil nil nil nil nil nil nil nil nil nil]
                         [nil :xx :xx :xx :xx :xx :xx :xx :xx nil]
                         [nil :xx :xx :xx :xx :xx :xx :xx :xx nil]
                         [nil :xx :xx :xx :xx :xx :xx :xx :xx nil]
                         [nil :xx :xx :xx :xx :xx :xx :xx :xx nil]
                         [nil :xx :xx :xx :xx :xx :xx :xx :xx nil]]
                :lives  3})

;; Game state.
(def world-atom (r/atom new-world))


;; -------------------------
;; State manipulation functions

(defn block->pos
  "Calculate block rectangle."
  [[row column]]
  (let [width  (/ 1 block-columns)
        height (/ 1 block-rows)]
    [(+ (* column width) block-margin)
     (+ (* row height) block-margin)
     (- (* (inc column) width) block-margin)
     (- (* (inc row) height) block-margin)]))

(defn pos->block
  "Get block row/column for given coordinates."
  [blocks x y]
  (let [bump    (/ ball-size 200)
        ball-x1 (- x bump)
        ball-y1 (- y bump)
        ball-x2 (+ x bump)
        ball-y2 (+ y bump)]
    (loop [row 0]
      (when (< row block-rows)
        (if-let [result (loop [column 0]
                          (when (< column block-columns)
                            (let [[x1 y1 x2 y2] (block->pos [row column])]
                              (if (and (get-in blocks [row column])
                                       (or (<= x1 ball-x1 x2)  (<= x1 ball-x2 x2))
                                       (or (<= y1 ball-y1 y2) (<= y1 ball-y2 y2)))
                                [row column]
                                (recur (inc column))))))]
          result
          (recur (inc row)))))))

(defn game-over? [{:keys [lives]}]
  (<= lives 0))

(defn move-paddle
  "Move paddle."
  [{{:keys [x width]} :paddle :as world} dx]
  (if (game-over? world)
    world
    (let [new-x     (- x dx)
          bump      (/ width 2)
          new-x     (-> new-x (max bump) (min (- 1 bump)))
          dx        (- new-x x)
          new-world (assoc-in world [:paddle :x] new-x)]
      (if (-> world :ball :on-paddle)
        (update-in new-world [:ball :x] + dx)
        new-world))))

(defn tap-paddle
  "Handle click events."
  [world]
  (assoc-in world [:ball :on-paddle] false))

(defn move-ball
  "Move ball on step."
  [{{:keys [x y dx dy on-paddle]} :ball :as world}]
  (if on-paddle
    world
    (let [nx (+ x dx)
          ny (+ y dy)]
      (-> world
          (assoc-in [:ball :x] (-> nx (max 0) (min 1)))
          (assoc-in [:ball :y] (-> ny (max 0) (min 1)))))))

(defn bounce-ball-walls
  "Bounce ball when hitting a wall."
  [{{:keys [x y dx dy]} :ball :as world}]
  (-> world
      (assoc-in [:ball :dx] (if (or (<= x (/ ball-size 200))
                                    (>= x (- 1 (/ ball-size 200))))
                              (* -1 dx)
                              dx))
      (assoc-in [:ball :dy] (if (<= y (/ ball-size 200))
                              (* -1 dy)
                              dy))))

(defn bounce-angle
  "Determine bounce angel from position on paddle."
  [{:keys [width]} x]
  (* (- (* (- 180 (* 2 angle-constraint)) (/ x width))
        (- 90 angle-constraint))
     (/ math/pi 180)))

(defn bounce-ball-paddle
  "Bounce ball on paddle when paddle hit or take a live when ball passed
  paddle."
  [{:keys                               [ball paddle]
    {:keys [x y dx dy speed on-paddle]} :ball :as world}]
  (let [bump      (-> paddle :width (/ 2))
        paddle-x1 (-> paddle :x (- bump))
        paddle-x2 (-> paddle :x (+ bump))]
    (if (and (< paddle-x1 x paddle-x2)
             (<= ball-on-paddle-y y)
             (< y 1))
      (let [angle (bounce-angle paddle (- x paddle-x1))
            ndx   (* speed (math/sin angle))
            ndy   (* -1 speed (math/cos angle))]
        (-> world
            (assoc-in [:ball :y] ball-on-paddle-y)
            (assoc-in [:ball :dx] ndx)
            (assoc-in [:ball :dy] ndy)))
      world)))

(defn bounce-ball-blocks
  "Bounce ball off blocks and kill blocks when hit."
  [{:keys         [blocks]
    {:keys [x y]} :ball, :as world}]
  (let [coor (pos->block blocks x y)]
    (if-let [block (and coor (get-in blocks coor))]
      (let [[x1 y1 x2 y2] (block->pos coor)
            dist          (fn [a b] (- (max a b) (min a b)))
            dist-x        (min (dist x x1) (dist x x2))
            dist-y        (min (dist y y1) (dist y y2))]
        (-> world
            (assoc-in (into [:blocks] coor) nil)
            (update-in [:ball (if (<= dist-x dist-y) :dx :dy)] * -1)))
      world)))

(defn bounce-ball
  "Do all ball bounce stuff."
  [{{:keys [on-paddle]} :ball :as world}]
  (if on-paddle
    world
    (-> world
        bounce-ball-walls
        bounce-ball-paddle
        bounce-ball-blocks)))

(defn take-live
  "Take live when ball passed paddle."
  [{{:keys [x y on-paddle]} :ball :keys [paddle] :as world}]
  (if (or on-paddle (< y 1))
    world
    (-> world
        (update :lives dec)
        (assoc-in [:ball :on-paddle] true)
        (assoc-in [:ball :x] (+ (:x paddle) (* .25 (:width paddle))))
        (assoc-in [:ball :y] ball-on-paddle-y))))

(defn tick
  "Move and bounce ball stuff."
  [world]
  (if (game-over? world)
    world
    (-> world
        move-ball
        bounce-ball
        take-live)))


;; -------------------------
;; Event handlers

(defn tick! []
  (swap! world-atom tick))

(defn new-game! []
  (reset! world-atom new-world))

(let [previous-mouse-x-atom (atom nil)
      paddle-sensitivity {"mousemove" 500
                          "touchmove" 250}]
  (defn move-paddle!
    "Handle mouse/touch move events."
    [e]
    (let [x (.-screenX e)]
      (when-let [previous-mouse-x @previous-mouse-x-atom]
        (when-let [sensitivity (paddle-sensitivity (.-type e))]
          (let [dx (/ (- previous-mouse-x x) sensitivity)]
            (swap! world-atom move-paddle dx))))
      (reset! previous-mouse-x-atom x)))

  (defn touchend-paddle!
    [e]
    (reset! previous-mouse-x-atom nil)))

(defn tap-paddle!
  "Handle mouse/touch click events."
  [_]
  (swap! world-atom tap-paddle))


;; -------------------------
;; Views

(defn render-ball
  [{:keys [x y]}]
  [:div.ball {:style {:width  (str ball-size "%")
                      :height (str ball-size "%")
                      :left   (str (-> x (* 100) (- (/ ball-size 2))) "%")
                      :top    (str (-> y (* 100) (- (/ ball-size 2))) "%")}}])

(defn render-block
  [row column block]
  (let [[x1 y1 x2 y2] (block->pos [row column])]
    [:div.block {:key   (str "b" row "-" column)
                 :style {:left   (str (* x1 100) "%")
                         :top    (str (* y1 100) "%")
                         :right  (str (- 100 (* x2 100)) "%")
                         :bottom (str (- 100 (* y2 100)) "%")}}
     " "]))

(defn render-world
  []
  (let [world                              @world-atom
        {:keys [paddle ball blocks lives]} world]
    [:div.game
     [:div.lives (repeat lives "🖤")]

     [:div.world
      (render-ball ball)

      (for [[i row] (map-indexed vector blocks)]
        (for [[j block] (map-indexed vector row)]
          (when block (render-block i j block))))

      [:div.paddle {:style {:width (str (-> paddle :width (* 100)) "%")
                            :left  (str (-> paddle :x (- (-> paddle :width (/ 2))) (* 100)) "%")}}]

      (when (game-over? world)
        [:h2.game-over [:a {:onClick new-game!} "GAME OVER!"]])]]))

(defn render-main []
  [:main [:h1 "Breakout!"]
   (render-world)])


;; -------------------------
;; Initialize

(defn mount-root []
  (r/render [render-main] (.getElementById js/document "app")))

(defn init! []
  (events/listen js/document (to-array [event-type/MOUSEMOVE event-type/TOUCHMOVE]) move-paddle!)
  (events/listen js/document event-type/TOUCHEND touchend-paddle!)
  (events/listen js/document (to-array [event-type/CLICK event-type/TOUCHSTART]) tap-paddle!)
  (js/setInterval tick! 25)
  (mount-root))
