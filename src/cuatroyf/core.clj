(ns cuatroyf.core
  (:import java.awt.event.KeyEvent)
  (:require [quil.core :exclude state]
            [clojure.string :as st]))

(def valid-keys
  {KeyEvent/VK_UP    :up
   KeyEvent/VK_DOWN  :down
   KeyEvent/VK_LEFT  :left
   KeyEvent/VK_RIGHT :right
   \w                :up
   \s                :down
   \a                :left
   \d                :right})

(defn str->matrix
  [string]
  (remove nil?
          (mapcat
           (fn [y line]
             (map-indexed (fn [x char]
                            (when-not (= char \space) [[x y] char]))
                          line))
           (iterate inc 0)
           (st/split-lines (if (= (first string) \newline)
                             (apply str (rest string))
                             string)))))

(defmacro defshape
  [name string]
  `(def ~name (str->matrix ~string)))

(def location (atom [0 10]))

(def moves
  {:up    [0 -1]
   :down  [0 1]
   :left  [-1 0]
   :right [1 0]
   :still [0 0]})

(defshape asdf "
 _
 0
/O\\
 |")

(defn draw-shape
  [mat [x y] & [colors-fn]]
  (with-translation [(* x 11) (* y 22)]
    (doseq [[[x y] c] mat]
      (apply fill (if-let [color (and colors-fn (colors-fn [x y]))] color [255]))
      (text-char c (* x 11) (* y 22)))))

(defn shape-states
  [start-states]
  (let [states (atom start-states)]
    (fn []
      (apply draw-shape (first (if @states
                                 @states
                                 (swap! states #([_] start-states)))))
      (swap! states rest))))

(defn rand-coll
  [n & elems]
  (repeatedly n #(rand-nth (flatten elems))))

(defn space
  [x y w h density]
  (let [dens (/ h density)
        s (apply str
                 (apply concat
                        (repeatedly h
                                    #(concat (rand-coll w "." (repeat dens " "))
                                             [\newline]))))]
    (shape-states
     (map vector
          (reductions
           (fn [s last-col]
             (apply str
                    (map #(apply str (concat (rest %1) %2 [\newline]))
                         (st/split-lines s)
                         last-col)))
           s
           (repeatedly #(rand-coll h "." (repeat dens " "))))
          [x y]))))

(def back-space (space 0 8 88 10 1))

(def state
  (atom {:scene []
         :draw (fn [] (:scene @state))}))

(remove #{{:a 9} :b} [{:a (+ 4 5)} :b])

(defn transform!
  [selector f]
  (->> coll
       (filter #((:selector %)))

       ))

(up)


(defn game
  []
  (sketch-start
   (sketch
    :title       "430 y Capitan F en..."
    :setup       (fn []
                   (smooth)
                   (frame-rate 20)
                   (background 0))
    :draw        (fn [] ((:draw @state)))
    :key-pressed (fn []
                   (let [raw-key         (raw-key)
                         the-key-code    (key-code)
                         the-key-pressed (if (= processing.core.PConstants/CODED (int raw-key))
                                           the-key-code
                                           raw-key)]
                     ((:key-pressed @state) (get valid-keys the-key-pressed))))
    :size        [640 480])))
