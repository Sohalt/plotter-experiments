(ns mountains
  (:require [quil.core :as q]
            [solenoid.controls :as c]
            [solenoid.server :as ss]))

(ss/serve!) ;; webpage served on port 9876, or lowest available port starting at 8000.

(def white 255)

(def r (c/letcontrols [x {:type :num
                          :display-name "seed"
                          :value 1}]
         (java.util.Random. x)))

(def seed (c/letcontrols [x {:type :num
                             :display-name "seed2"
                             :value 1}]
         x))

(defn rand
  ([] (.nextDouble @r))
  ([n] (.nextInt @r n)))

(defn rand-between [min max]
  (+ min (rand (- max min))))

(def sun-radius
  (c/letcontrols [x {:type :slider :value 20}] x))

(def sun-pos
  (c/letcontrols [x {:type :slider :value 20}
                  y {:type :slider :value 20}]
    {:x x :y y}))

(defn sun []
  (let [r @sun-radius #_(rand-between 20 40)
        {:keys [x y]} @sun-pos]
    (q/ellipse x y #_#_ (rand-between 10 50) (rand-between 10 50) r r)))

(defn ridge []
  (reset! r (java.util.Random. @seed))
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
  (q/frame-rate 30))

(q/defsketch mountains
  :draw draw
  :setup setup)
