(ns plot
  (:require [clojure.string :as str]))

(defn goto [[x y]]
  (format "PA %s,%s;" x y))

(defn pen-down []
  "PD;")

(defn pen-up []
  "PU;")

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
