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
  "Given a point, returns the population at that point. Since the image is grayscale, this is simply any of the rgb values"
  [[x y]]
  (if (and (< -1 x size-x) (< -1 y size-y))
    (red (nth @heightmap (+ x (* y size-x))))
    0))

;
; The guts of road drawing AKA the interesting, gnarly bits
;

(def road-length 10.0)

(def dummy-road {:point [321 420] :angle 91})

(defn endpoint-for
  ([{:keys [point angle]}] (endpoint-for point angle road-length))
  ([[x y] angle length]
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

(defn offsets-to-check
  "Returns all angle/length pairs to check when building a new segment"
  []
  (for [angle (map #(+ (* 45 %) (round (random -5 5))) (range 8))
        length (map #(* (inc %) road-length) (range 9))]
    {:angle angle :length length}))

(defn most-populous-offset
  "Given a point, returns the angle/length pair towards the most populous destination"
  [p]
  (reduce (fn [offset1 offset2]
            (if (< (population-at (endpoint-for p (:angle offset1) (:length offset1)))
                   (population-at (endpoint-for p (:angle offset2) (:length offset2))))
              offset2
              offset1)) (offsets-to-check)))

(defn next-generation
  "Given a set of roads, returns the next generation of roads available"
  [roads]
  (if (empty? roads)
    (let [point (take 2 (repeatedly #(round (random 0 size-x))))]
      [{:point point :angle (:angle (most-populous-offset point))}])
    roads))

(defn mouse-pressed
  "Upon a mouse press, outputs the population at that coordinate. Used for inspection"
  []
  (let [point [(mouse-x) (mouse-y)]]
    (println (str "Population at (" point "): " (population-at point)))
    (println (str "Most populous angle:" (:angle (most-populous-offset point))))))

(defn draw-roads
  "Draws all roads for the current generation"
  [all-roads]
  (stroke 255 0 0 255)
  (stroke-weight 2)
  (dorun
    (for [road all-roads]
      (let [p1 (:point road)
            p2 (endpoint-for road)]
        (apply line (flatten [p1 p2]))))))

(defn key-pressed
  "When the up arrow is pressed, advance the sim and redraw"
  []
  (if (= (key-as-keyword) :up)
    (do
      (swap! roads next-generation)
      (draw-background)
      (draw-roads @roads))))

(defn launch
  "Starts the simulation"
  []
  (defsketch cloville-sketch :title "Cloville" :draw draw :setup setup :size [size-x size-y] :mouse-pressed mouse-pressed :key-pressed key-pressed))
