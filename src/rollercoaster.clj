(ns rollercoaster
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [solenoid.controls :as c]
            [solenoid.server :as ss]))

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

(def c1 (c/letcontrols [amp (/ h 4)
                        offset-x -200
                        offset-y (/ h 2)
                        period 50]
           {:amp amp
            :offset-x offset-x
            :offset-y offset-y
            :period period}))
(def c2 (c/letcontrols [amp (/ h 5)
                        offset-x -200
                        offset-y (/ h 4)
                        period 40]
           {:amp amp
            :offset-x offset-x
            :offset-y offset-y
            :period period}))

(defn track [x] ((add (sin @c1)
                      (sin @c2)) x))

(defn path
  ([points] (path points {}))
  ([points {:keys [close?] :or {close? true}}]
   (let [start-points points
         end-points (cond-> (vec (rest points))
                      close? (conj (first points)))]
     (map (fn [start end] [start end])
          start-points end-points))))

(defn gondola [[top-x top-y]]
  (let [roof-height 5
        railing-height 8
        width 20
        height 20
        left (- top-x (/ width 2))
        right (+ top-x (/ width 2))
        roof-bottom (+ top-y roof-height)
        bottom (+ top-y height)
        railing (- bottom railing-height)]
    (concat (path [[top-x top-y]
                   [left roof-bottom]
                   [left bottom]
                   [right bottom]
                   [right roof-bottom]])
            [[[left roof-bottom] [right roof-bottom]]
             [[left railing] [right railing]]])))

(def tau (* 2 Math/PI))

(defn point-on-circle [[cx cy] r angle]
  [(+ cx (* r (Math/sin angle))) (+ cy (* r (Math/cos angle)))])

(def num-segments 20)
(defn circle [[cx cy] r]
  (->> (range 0 tau (/ tau num-segments))
       (map (fn [angle] (point-on-circle [cx cy] r angle)))
       (path)))

(defn ferris-wheel
  ([[cx cy] r] (ferris-wheel [cx cy] r 0))
  ([[cx cy] r t]
   (let [num-gondolas 10
         points-on-circle (map (fn [angle]
                                 (point-on-circle [cx cy] r (+ t angle)))
                               (range 0 tau (/ tau num-gondolas)))
         spokes (map (fn [p] [[cx cy] p]) points-on-circle)
         gondolas (mapcat gondola points-on-circle)]
     (concat (circle [cx cy] r)
             spokes
             gondolas))))

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

(defn roller-coaster []
  (concat (trace-segments track)
          (scaffolding {})))

(defn visible-point? [[x y]]
  (and (<= 0 x w) (<= 0 y h)))

(defn visible-line? [[start end]]
  (or (visible-point? start) (visible-point? end)))

(defn draw [{:keys [lines]}]
  (q/background 255)
  (doall (map (partial apply q/line) lines)))

(defn setup []
  {:lines (concat (roller-coaster)
                  (ferris-wheel [(/ w 1.2) (/ h 2.5)] 100))})

(defn update-state [_]
  {:lines (concat (roller-coaster)
                  (ferris-wheel [(/ w 1.2) (/ h 2.5)] 100))})

(defn setup-animation-state []
  (q/frame-rate 30)
  {:lines (concat (roller-coaster)
                  (ferris-wheel [(/ w 1.2) (/ h 2.5)] 100))
   :t 0})

(defn update-animation-state [{:keys [t]}]
  {:t (inc t)
   :lines (concat (roller-coaster)
                  (ferris-wheel [(/ w 1.2) (/ h 2.5)] 100 (/ t 40)))})

(defn setup-plot-state []
  (q/frame-rate 60)
  {:lines []
   :lines-todo (filter visible-line? (concat (roller-coaster)
                                             (ferris-wheel [(/ w 1.2) (/ h 2.5)] 100)))})

(defn update-plot-state [{:keys [lines lines-todo]}]
  {:lines (conj lines (first lines-todo))
   :lines-todo (rest lines-todo)})

(comment
  (q/defsketch static
    :size [w h]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode])
  (do (ss/serve!)
      (clojure.java.browse/browse-url "http://localhost:9876"))

  (q/defsketch animated
    :size [w h]
    :setup setup-animation-state
    :draw draw
    :update update-animation-state
    :middleware [m/fun-mode])

  (q/defsketch plot
    :size [w h]
    :setup setup-plot-state
    :draw draw
    :update update-plot-state
    :middleware [m/fun-mode])
  )
