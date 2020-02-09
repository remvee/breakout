(ns breakout.core
  (:require [goog.events :as events]
            [goog.events.EventType :as event-type]
            [reagent.core :as r]))

(def paddle-sensitivity 500)

(def block-columns 10)
(def block-rows 10)

(def new-world {:paddle {:x .5, :width .1}
                :ball   {:x .525, :y 0.97, :dx 0, :dy .02, :speed .02, :on-paddle true}
                :blocks (into [] (repeat 5 (into [] (repeat 10 :red))))
                :lives  3})

(def world-atom (r/atom new-world))

(defn block->pos [row column]
  (let [width  (/ 1 block-columns)
        height (/ 1 block-rows)]
    [(+ (* column width) .005)
     (+ (* row height) .005)
     (- (* (inc column) width) .01)
     (- (* (inc row) height) .01)]))

(defn pos->block [x y]
  (let [width  (/ 1 block-columns)
        height (/ 1 block-rows)]
    (loop [row 0]
      (when (< row block-rows)
        (if-let [result (loop [column 0]
                          (when (< column block-columns)
                            (let [[x1 y1 x2 y2] (block->pos row column)]
                              (if (and (<= x1 x x2)
                                       (<= y1 y y2))
                                [row column]
                                (recur (inc column))))))]
          result
          (recur (inc row)))))))

(defn game-over? [{:keys [lives]}]
  (<= lives 0))

(defn update-paddle [{{:keys [x width]} :paddle :as world} delta-pixels]
  (if (game-over? world)
    world
    (let [dx        (/ delta-pixels paddle-sensitivity)
          new-x     (- x dx)
          bump      (/ width 2)
          new-x     (-> new-x (max bump) (min (- 1 bump)))
          dx        (- new-x x)
          new-world (assoc-in world [:paddle :x] new-x)]
      (if (-> world :ball :on-paddle)
        (update-in new-world [:ball :x] + dx)
        new-world))))

(defn tap-paddle [world]
  (assoc-in world [:ball :on-paddle] false))

(defn move-ball [{{:keys [x y dx dy on-paddle]} :ball :as world}]
  (if on-paddle
    world
    (let [nx (+ x dx)
          ny (+ y dy)]
      (-> world
          (assoc-in [:ball :x] (-> nx (max 0) (min 1)))
          (assoc-in [:ball :y] (-> ny (max 0) (min 1)))))))

(defn bounce-ball-walls [{{:keys [x y dx dy]} :ball :as world}]
  (-> world
      (assoc-in [:ball :dx] (if (or (= x 0) (= x 1))
                              (* -1 dx)
                              dx))
      (assoc-in [:ball :dy] (if (or (= y 0) #_(= y 1))
                              (* -1 dy)
                              dy))))

(def angle-constraint 20)

(defn bounce-angle [{:keys [width]} x]
  (* (- (* (- 180 (* 2 angle-constraint)) (/ x width))
        (- 90 angle-constraint))
     (/ (.-PI js/Math) 180)))

(defn bounce-ball-paddle [{:keys                     [ball paddle]
                           {:keys [x y dx dy speed]} :ball :as world}]
  (let [bump      (-> paddle :width (/ 2))
        paddle-x1 (-> paddle :x (- bump))
        paddle-x2 (-> paddle :x (+ bump))]
    (cond
      (and (< paddle-x1 x paddle-x2)
           (<= .97 y .9999))
      (let [angle (bounce-angle paddle (- x paddle-x1))
            ndx   (* speed (.sin js/Math angle))
            ndy   (* -1 speed (.cos js/Math angle))]
        (-> world
            (assoc-in [:ball :y] .97)
            (assoc-in [:ball :dx] ndx)
            (assoc-in [:ball :dy] ndy)))

      (>= y 1)
      (-> world
          (update :lives dec)
          (assoc-in [:ball :on-paddle] true)
          (assoc-in [:ball :x] (- paddle-x2 (/ bump 2)))
          (assoc-in [:ball :y] .97))

      :else
      world)))

(defn bounce-ball-blocks [{:keys         [blocks]
                           {:keys [x y]} :ball, :as world}]
  (let [coor (pos->block x y)]
    (if-let [block (and coor (get-in blocks coor))]
      (assoc-in world (into [:blocks] coor) nil)
      world)))

(defn bounce-ball [world]
  (-> world
      bounce-ball-walls
      bounce-ball-paddle
      bounce-ball-blocks))

(defn tick [world]
  (if (game-over? world)
    world
    (-> world
        move-ball
        bounce-ball)))

(defn tick! []
  (swap! world-atom tick))

(defn new-game! []
  (reset! world-atom new-world))

;; -------------------------
;; Views

(defn pos-style [x y width height]
  {:left (str (-> x (* 100) (- (/ width 2))) "%")
   :top  (str (-> y (* 100) (- (/ height 2))) "%")})

(defn render-ball [{:keys [x y]}]
  [:div.ball {:style (into {:width "2%", :height "2%"} (pos-style x y 2 2))}])

(defn render-block [row column block]
  (let [[x1 y1 x2 y2] (block->pos row column)]
    [:div.block {:key   (str "b" row "-" column)
                 :style {:left   (str (* x1 100) "%")
                         :top    (str (* y1 100) "%")
                         :right  (str (- 100 (* x2 100)) "%")
                         :bottom (str (- 100 (* y2 100)) "%")}}
     block]))

(defn render-world []
  (let [world                              @world-atom
        {:keys [paddle ball blocks lives]} world]
    [:div.world {:class (when (game-over? world) "game-over")}
     [:div.lives (str lives)]
     (when (game-over? world)
       [:h2.game-over [:a {:onClick new-game!} "GAME OVER!"]])

     (for [[i row] (map-indexed vector blocks)]
       (for [[j block] (map-indexed vector row)]
         (when block (render-block i j block))))

     (render-ball ball)
     [:div.paddle {:style {:width (str (-> paddle :width (* 100)) "%")
                           :left  (str (-> paddle :x (- (-> paddle :width (/ 2))) (* 100)) "%")}}]]))

(defn home-page []
  [:main [:h1 "Breakout!"]
   (render-world)])

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (events/listen js/document (to-array [event-type/MOUSEMOVE event-type/TOUCHMOVE])
                 (let [previous-mouse-x-atom (atom nil)]
                   (fn [e]
                     (let [x (.-screenX e)]
                       (when-let [previous-mouse-x @previous-mouse-x-atom]
                         (swap! world-atom update-paddle (- previous-mouse-x x)))
                       (reset! previous-mouse-x-atom x)))))
  (events/listen js/document (to-array [event-type/CLICK event-type/TOUCHSTART])
                 (fn [e]
                   (swap! world-atom tap-paddle)))

  (js/setInterval tick! 25)
  (mount-root))
