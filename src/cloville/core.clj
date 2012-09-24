(ns cloville.core
  (:use [quil.core :exclude [alpha red green blue]]
        [quil.helpers.seqs :only [range-incl]]
        [quil.helpers.calc :only [mul-add]])
  (:require [cloville.roads :as roads]))

(def size-x 600)
(def size-y 600)

(def heightmap (delay (load-pixels) (pixels)))

(defn alpha [rgba] (bit-and (bit-shift-right rgba 24) 255))
(defn red [rgba] (bit-and (bit-shift-right rgba 16) 255))
(defn green [rgba] (bit-and (bit-shift-right rgba 8) 255))
(defn blue [rgba] (bit-and rgba 255))

(defn population-at [x y]
  (red (nth @heightmap (+ x (* y size-x)))))

(defn pressed []
  (let [x (mouse-x)
        y (mouse-y)]
  (println (str "Population at (" x "," y "): " (population-at x y)))))

(defn draw-background []
  (background 255)
  (dorun
   (let [x-start 5
         y-start 5]
     (for [y (range-incl (height))
           x (range-incl (width))]
       (let [x-noise (mul-add x 0.01 x-start)
             y-noise (mul-add y 0.01 y-start)
             alph    (* 255 (noise x-noise y-noise))]
         (stroke-weight 1)
         (stroke-int 0 alph)
         (line x y (inc x) (inc y))))))
  @heightmap)

(defn setup []
  (smooth)
  (frame-rate 1)
  (draw-background))

(def pending-fn (atom nil))

(defn invoke-now
  "Dispatches a function to be executed by the appropriate thread, etc."
  [f]
  (let [p (promise)]
    (reset! pending-fn #(deliver p (f)))
    @p))

(defn invoke-pending-call []
  (if @pending-fn
    (do (@pending-fn)
      (reset! pending-fn nil))))

(defn draw []
  (draw-background)
  (roads/draw-roads)
  (invoke-pending-call))

(defn launch []
  (defsketch cloville-sketch :title "Cloville" :draw (var draw) :setup setup :size [size-x size-y] :mouse-pressed pressed))
