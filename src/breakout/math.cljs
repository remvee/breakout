(ns breakout.math)

(def ^:const pi (.-PI js/Math))

(def sin #(.sin js/Math %))
(def cos #(.cos js/Math %))
