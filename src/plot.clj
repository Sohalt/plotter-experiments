(ns plot
  (:require [clojure.string :as str]))

(defn goto [[x y]]
  (format "PA %s,%s;" x y))

(defn pen-down []
  "PD;")

(defn pen-up []
  "PU;")

(defn line [from to]
  (goto from)
  (pen-down)
  (goto to)
  (pen-up))

(defn normalize-line
  "flip start and endpoint of a line so that start always has a lower x than end,
  i.e. the line goes from left to right.
  If it's a vertical line (i.e. start x and end x are the same),
  sort so it goes from bottom to top"
  [[[sx sy] [ex ey]]]
  (cond (< sx ex) [[sx sy] [ex ey]]
        (> sx ex) [[ex ey] [sx sy]]
        (> sy ey) [[ex ey] [sx sy]]
        :else [[sx sy] [ex ey]]))

(defn distance [[x1 y1] [x2 y2]]
  (Math/sqrt (+ (Math/pow (- x2 x1) 2)
                (Math/pow (- y2 y1) 2))))

(defn angle [[x1 y1] [x2 y2]]
  (Math/atan2 (- y2 y1) (- x2 x1)))

(defn optimize-lines [lines]
  (let [norm (map normalize-line lines)
        sorted (sort-by ffirst norm)]
    ;;TODO
    ;;find closest line
    ;;when multiple: choose the one with the least change in angle
    sorted))

(defn square [center w]
  (let [[cx cy] center
        wh (/ w 2)
        tl [(- cx wh) (- cy wh)]
        tr [(+ cx wh) (- cy wh)]
        bl [(- cx wh) (+ cy wh)]
        br [(+ cx wh) (+ cy wh)]]
    (str (goto tl)
         (pen-down)
         (goto tr)
         (goto br)
         (goto bl)
         (goto tl)
         (pen-up))))

(defn preamble []
  (str/join "\n" ["IN;" "PS 2;"]))

(defn plot [& s]
  (doseq [line (concat [(preamble)] s)]
    (println line)))
