(ns fataliste.gui
  (:import (java.awt Color Dimension Font)
           (javax.swing JFrame JPanel Timer)
           (java.awt.event ActionListener KeyListener))
  (:gen-class))

(def panel-size {:w 800 :h 600})
(def directions [:left :up :right :down :jump :none])
(def arrow-color {:left Color/magenta :up Color/yellow :right Color/green :down Color/cyan :jump Color/red :none Color/black})
(def font-size 64)
(defn common-font
  ([] (common-font font-size))
  ([size] (Font. "Yu Gothic" Font/BOLD size)))
(defn abs [x] (max x (- x)))

(defn paint-arrow
  [g direction x y]
  (let [arrow (direction {:left "←" :up "↑" :right "→" :down "↓" :jump "⤴︎" :none ""})]
    (doto g
      (.setFont (common-font))
      (.setColor (arrow-color direction))
      (.drawString arrow (int x) (int y)))))

(defn paint-player
  [g p]
  (let [player-pos-f {:left {:x (- (/ (- (:w panel-size) font-size) 2) font-size) :y (* (:h panel-size) 3/4) :large font-size}
                      :up {:x (/ (- (:w panel-size) font-size) 2) :y (- (* (:h panel-size) 3/4) font-size) :large font-size}
                      :right {:x (+ (/ (- (:w panel-size) font-size) 2) font-size) :y (* (:h panel-size) 3/4) :large font-size}
                      :down {:x (/ (- (:w panel-size) font-size) 2) :y (+ (* (:h panel-size) 3/4) font-size) :large font-size}
                      :jump {:x (/ (- (:w panel-size) (* font-size 3/2)) 2) :y (* (- (:h panel-size) (/ font-size 2)) 3/4) :large (* font-size 3/2)}
                      :none {:x (/ (- (:w panel-size) font-size) 2) :y (* (:h panel-size) 3/4) :large font-size}}
        player-pos (player-pos-f (:dir p))]
    (doto g
      (.setColor (arrow-color (:dir p)))
      (.fillRect (:x player-pos) (:y player-pos) (:large player-pos) (:large player-pos)))))

(defn paint-game
  [g c arrows]
  (let [c2 (* 12 (- (rem c 60) 30))]
    (doto g
      (.setColor Color/blue)
      (.fillArc (/ (- (:w panel-size) font-size) 2) (/ (- (/ (:h panel-size) 2) font-size) 2) font-size font-size (max 0 c2) (- 360 (abs c2)))
      (paint-arrow (nth arrows (quot c 30)) (/ (- (:w panel-size) font-size) 2) (+ font-size (/ (- (/ (:h panel-size) 2) font-size) 2))))
    (when (and (> c 60) (>= (rem c 30) 15))
      (doto g
        (paint-arrow (nth arrows (- (quot c 30) 2)) (/ (- (:w panel-size) font-size) 2) (+ font-size (* (:h panel-size) 3/4)))))))

(defn paint-score
  [g sc]
  (doto g
    (.setFont (common-font 20))
    (.setColor Color/yellow)
    (.drawString (str (:score sc)) 0 20))
  (when (:over sc)
    (doto g
      (.setFont (common-font))
      (.setColor Color/red)
      (.drawString "GAME OVER!!!" (/ (:w panel-size) 4) (+ font-size (/ (- (:h panel-size) font-size) 2))))))

(defn paint-game-play
  [g c a p s]
  (doto g
    (.setColor Color/black)
    (.fillRect 0 0 (:w panel-size) (/ (:h panel-size) 2))
    (.setColor Color/white)
    (.fillRect 0 (/ (:h panel-size) 2) (:w panel-size) (/ (:h panel-size) 2))
    (paint-player p)
    (paint-game c a)
    (paint-score s)))

(defn paint-game-title
  [g]
  (doto g
    (.setColor Color/black)
    (.fillRect 0 0 (:w panel-size) (:h panel-size))
    (.setFont (common-font))
    (.setColor Color/yellow)
    (.drawString "le fataliste" (/ (:w panel-size) 4) (+ font-size (/ (- (:h panel-size) font-size) 2)))
    (.setFont (common-font 38))
    (.drawString "PRESS [Enter] TO START" (- (/ (:w panel-size) 4) 60) (+ font-size (/ (- (:h panel-size) font-size) 2) 100))))

(defn game-panel
  "GUIを管理する関数"
  []
  (let [counter (atom 0)
        arrows (atom (repeatedly #(rand-nth directions)))
        player (atom {:dir :none})
        score (atom {:score 0 :over true})
        mode (atom :title)]
    (proxy [JPanel ActionListener KeyListener] []
      (paintComponent [g]
        (case @mode
          :title (paint-game-title g)
          :play (paint-game-play g @counter @arrows @player @score)))
      (actionPerformed [e]
        (when-not (:over @score)
          (do
            (swap! counter inc)
            (when (= 15 (rem @counter 30))
              (if (or (< @counter 60) (= (nth @arrows (- (quot @counter 30) 2)) :none) (= (:dir @player) (nth @arrows (- (quot @counter 30) 2))))
                (swap! score #(assoc % :score (inc (:score @score))))
                (swap! score #(assoc % :over true))))))
        (.repaint this))
      (keyPressed [e]
        (case (.getKeyCode e)
          37 (swap! player #(assoc % :dir :left))
          38 (swap! player #(assoc % :dir :up))
          39 (swap! player #(assoc % :dir :right))
          40 (swap! player #(assoc % :dir :down))
          32 (swap! player #(assoc % :dir :jump))
          10 (case @mode
               :title (do (reset! arrows (repeatedly #(rand-nth directions)))
                          (reset! counter 0)
                          (reset! score {:score 0 :over false})
                          (reset! mode :play))
               :play (when (:over @score) (reset! mode :title)))
          nil))
      (keyTyped [e])
      (keyReleased [e]
        (swap! player #(assoc % :dir :none)))
      (getPreferredSize []
        (Dimension. (:w panel-size) (:h panel-size))))))

(defn game-play
  "fpsを引数にとって，ゲームを開始する関数"
  [fps]
  (let [frame (JFrame. "le fataliste")
        panel (game-panel)
        timer (Timer. (int (/ 1000 fps)) panel)]
    (doto panel
      (.setFocusable true)
      (.addKeyListener panel))
    (doto frame
      (.setDefaultCloseOperation javax.swing.WindowConstants/EXIT_ON_CLOSE)
      (.setResizable false)
      (.add panel)
      (.pack)
      (.setVisible true))
    (.start timer)))
