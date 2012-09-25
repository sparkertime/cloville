(ns cloville.core
  (:use [quil.core :exclude [alpha red green blue]]
        [quil.helpers.seqs :only [range-incl]]
        [quil.helpers.calc :only [mul-add]]))

(def heightmap (delay (load-pixels) (pixels)))

; noise functions are randomized one-per-startup, so this is safe to call repeatedly to reset the background noise to the same background
(defn draw-background
  "Renders a background of perlin noise, and primes the heightmap to access that background"
  []
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

(defn setup
  "Sets up the image with the background and some random initialization"
  []
  (smooth)
  (draw-background))

;
; Some exploration-via-repl assistance for the draw method
;

(def pending-fn (atom nil))

(defn invoke-now
  "Dispatches a function to be executed by the appropriate thread, etc."
  [f]
  (let [p (promise)]
    (reset! pending-fn #(deliver p (f)))
    @p))

(defn invoke-pending-call
  "Invokes a queued method call, if any"
  []
  (if @pending-fn
    (do (@pending-fn)
      (reset! pending-fn nil))))

(defn draw
  "Does nothing but check for pending invocations"
  []
  (invoke-pending-call))

;
; Some utility functions related to the state of the generated background
;

(def size-x 600)
(def size-y 600)

(defn alpha
  "a faster replacement for the built-in alpha function. Returns an integer between 0-255"
  [rgba]
  (bit-and (bit-shift-right rgba 24) 255))

(defn red
  "A faster replacement for the built-in red function. Returns an integer between 0-255"
  [rgba]
  (bit-and (bit-shift-right rgba 16) 255))

(defn green
  "A faster replacement for the built-in green function. Returns an integer between 0-255"
  [rgba]
  (bit-and (bit-shift-right rgba 8) 255))

(defn blue
  "A faster replacement for the built-in blue function. Returns an integer between 0-255"
  [rgba]
  (bit-and rgba 255))

(defn population-at
  "Given an x-y pair, returns the population at that point. Since the image is grayscale, this is simply any of the rgb values"
  [x y]
  (red (nth @heightmap (+ x (* y size-x)))))

;
; The guts of road drawing AKA the interesting, gnarly bits
;

(def length 10.0)

(def dummy-road {:x 321 :y 420 :angle 91})

(defn endpoint-for
  ([{:keys [x y angle]}] (endpoint-for x y angle))
  ([{:keys [x y angle]} length] (endpoint-for x y angle length))
  ([x y angle] (endpoint-for x y angle length))
  ([x y angle length]
     [(round (+ x (* length (sin (radians angle)))))
      (round (+ y (* length (cos (radians angle)))))]))

(def roads (atom []))

(defn local-constraints [road]
  "Checks the local constraints for a road and returns a new road that satisfies the local constraints, or null if none can be found. These local constraints include...
    * Checking roadway against water boundaries and truncating it up to a certain factor
    * Checking for intersections within a radius of the end segment and adjusting appropriately"
  [])

(defn new-roads []
  "Implementation of Global Contraints. Adds branches with delays in accordance to our global rules (attract to populations, follow global-rules (NY, Paris, SF, 'Basic'). Not entirely sure what this does yet" [])

(defn mouse-pressed
  "Upon a mouse press, outputs the population at that coordinate. Used for inspection"
  []
  (let [x (mouse-x)
        y (mouse-y)]
  (println (str "Population at (" x "," y "): " (population-at x y)))))

(defn draw-roads
  "Draws all roads for the current generation"
  []
  (stroke 255 0 0 255)
  (stroke-weight 2)
  (dorun
    (for [road @roads]
      (apply line road))))

(defn key-pressed
  "Advances the state of the simulation by one generation if the up arrow is pressed"
  []
  (if (= (key-as-keyword) :up)
    (draw-background)
    (draw-roads)))

(defn launch
  "Starts the simulation"
  []
  (defsketch cloville-sketch :title "Cloville" :draw draw :setup setup :size [size-x size-y] :mouse-pressed mouse-pressed :key-pressed key-pressed))
