(ns mountains
  (:require [quil.core :as q]))

(def white 255)

(defn rand-between [min max]
  (+ min (rand-int (- max min))))

(defn sun []
  (let [r (rand-between 20 40)]
    (q/ellipse (rand-between 10 50) (rand-between 10 50) r r)))

(defn ridge []
  (let [height #(rand-between (* 1/4 (q/height)) (* 3/4 (q/height)))]
    (loop [x 0 y (height)]
      (let [step (rand-between 20 40)
            x' (min (+ x step) (q/width))
            y' (height)]
        (q/stroke 1)
        (q/line x y x' y')
        (when (< x' (q/width))
          (recur x' y'))))))

(defn draw []
  (q/background white)
  (ridge)
  (sun))

(defn setup []
  (q/frame-rate 1))

(q/defsketch mountains
  :draw draw
  :setup setup)
