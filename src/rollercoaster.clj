(ns rollercoaster
  (:require [quil.core :as q]))

(def w 600)
(def h 400)

(def sample 10)

(defn trace-segments [f]
  (let [xs (range 0 w sample)
        ys (map f xs)
        xs-end (drop 1 xs)
        ys-end (drop 1 ys)]
    (->> (map (fn [x-start y-start x-end y-end]
            (when (and x-start y-start x-end y-end)
              [[x-start y-start] [x-end y-end]]))
          xs ys xs-end ys-end)
         (keep identity))))

(defn sin
  ([] (sin {}))
  ([{:keys [amp offset-x offset-y period]
     :or {amp 1
          offset-x 0
          offset-y 0
          period 1}}]
   (fn [x] (+ (* amp (Math/sin (/ (- x offset-x) period))) offset-y))))

(defn add [f1 f2]
  (fn [x] (+ (f1 x) (f2 x))))

(def track (let [ox -200]
             (add (sin {:amp (/ h 4)
                        :offset-x ox
                        :offset-y (/ h 2)
                        :period 50})
                  (sin {:amp (/ h 5)
                        :offset-x ox
                        :offset-y (/ h 4)
                        :period 40}))))

(defn vertical-line
  ([x]
   (vertical-line x h))
  ([x height]
   [[x h] [x height]]))

(defn line
  ([] (line {}))
  ([{:keys [slope offset]
     :or {slope 1 offset 0}}]
   (fn [x] (+ (* slope x) offset))))

(defn mask [m f]
  (fn [x] (let [y (f x)]
            (when (< (m x) y)
              y))))

(defn scaffolding [{:keys [d-x slope]
                    :or {d-x 20
                         slope 0.5}}]
  (let [xs (range 0 w d-x)
        vertical-segments (concat (map #(vertical-line % (track %)) xs)
                                  (map #(vertical-line % (track %)) (map (partial + 5) xs)))
        descending-f (map #(mask track (line {:slope (- slope) :offset %})) (range 0 1500 d-x))
        ascending-f (map #(mask track (line {:slope slope :offset %})) (range -1500 1500 d-x))
        ]
    (concat vertical-segments
            (mapcat trace-segments descending-f)
            (mapcat trace-segments ascending-f))))

(defn draw []
  (q/background 255)
  (doall (map (partial apply q/line) (trace-segments track)))
  (doall (map (partial apply q/line) (scaffolding {}))))

(q/defsketch rollercoaster
  :size [w h]
  :draw draw)
