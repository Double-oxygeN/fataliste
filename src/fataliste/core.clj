(ns fataliste.core
  (:require [fataliste.gui :as g])
  (:gen-class))

(defn -main
  [& args]
  (g/game-play 30))
