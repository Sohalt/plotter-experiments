(ns plotter-test
  (:require [clojure.string :as str]
   [rollercoaster :refer :all]))

(def scale-factor 5)

(defn plot-line [[[x y] [x' y']]]
  (let [[[x y] [x' y']] [[(* scale-factor x) (* scale-factor y)] [(* scale-factor x') (* scale-factor y')]]]
    (println (str "PA " x "," y ";"))
    (println "PD;")
    (println (str "PA " x' "," y' ";"))
    (println "PU;")))

(defn preamble []
  (str/join "\n" ["IN;" "SP 2;"]))

(defn trailer []
  (str/join "\n" ["SP0;"]))

#_ (def lines (trace-segments (line)))
(def lines (filter visible-line? (concat (roller-coaster)
                                         (ferris-wheel [474 217] 110))))

(defn -main []
  (println (preamble))
  (doseq [line lines]
    (plot-line line))
  (println (trailer)))
