(ns breakout.core
    (:require [goog.events :as events]
              [goog.events.KeyCodes :as key-codes]
              [goog.events.EventType :as event-type]
              [reagent.core :as r]))

(def paddle-sensitivity 500)

(def new-world {:paddle {:x .5, :width .1}
                :ball   {:x .5, :y 0.97, :dx 0, :dy -1, :speed .02}
                :blocks (repeat 10 (repeat 10 :red))})

(def world-atom (r/atom new-world))

(defn update-paddle [{{:keys [x width]} :paddle :as world} delta-pixels]
  (prn delta-pixels)
  (let [new-x (- x (/ delta-pixels paddle-sensitivity))
        bump  (/ width 2)
        new-x (-> new-x (max bump) (min (- 1 bump)))]
    (assoc-in world [:paddle :x] new-x)))

(defn move-ball [{{:keys [x y dx dy]} :ball :as world}]
  (let [nx (+ x dx)
        ny (+ y dy)]
    (-> world
        (assoc-in [:ball :x] (-> nx (max 0) (min 1)))
        (assoc-in [:ball :y] (-> ny (max 0) (min 1))))))

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
    (if (and (< paddle-x1 x paddle-x2)
             (<= .97 y 1))
      (let [angle (bounce-angle paddle (- x paddle-x1))
            ndx   (* speed (.sin js/Math angle))
            ndy   (* -1 speed (.cos js/Math angle))]
        (-> world
            (assoc-in [:ball :y] .97)
            (assoc-in [:ball :dx] ndx)
            (assoc-in [:ball :dy] ndy)))
      world)))

(defn bounce-ball [{{:keys [x y dx dy]} :ball :as world}]
  (-> world
      bounce-ball-walls
      bounce-ball-paddle))

(defn tick [world]
  (-> world
      move-ball
      bounce-ball))

(defn tick! []
  (swap! world-atom tick))

;; -------------------------
;; Views

(defn render-world []
  (let [{:keys [paddle ball blocks]} @world-atom]
    [:div.world
     [:div.ball {:style {:left (str (-> ball :x (* 100) (- 1)) "%")
                         :top  (str (-> ball :y (* 100) (- 1)) "%")}}]
     [:div.paddle {:style {:width (str (-> paddle :width (* 100)) "%")
                           :left  (str (-> paddle :x (- (-> paddle :width (/ 2))) (* 100)) "%")}}]]))

(defn home-page []
  [:div [:h2 "Breakout!"]
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

  (js/setInterval tick! 25)
  (mount-root))
