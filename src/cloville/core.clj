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

(defn endpoint-for
  ([{:keys [point angle]}] (endpoint-for point angle road-length))
  ([[x y] angle length]
   [(round (+ x (* length (sin (radians angle)))))
    (round (+ y (* length (cos (radians angle)))))]))

(defn local-constraints [road]
  "Checks the local constraints for a road and returns a new road that satisfies the local constraints, or null if none can be found. These local constraints include...
  * Checking roadway against water boundaries and truncating it up to a certain factor
  * Checking for intersections within a radius of the end segment and adjusting appropriately"
  [])

(defn new-roads []
  "Implementation of Global Contraints. Adds branches with delays in accordance to our global rules (attract to populations, follow global-rules (NY, Paris, SF, 'Basic'). Not entirely sure what this does yet" [])

(defn offsets-to-check
  "[] - Returns all angle/length pairs to check when building a new segment
   [original-angle contraining-span] - Given an angle, returns all checkable angles within (+- constraining-span) degrees"
  ([] (offsets-to-check 0 360)) ;note that this currently adds both 0 and 360. However, that's filtered out in subsequent fns
  ([original-angle constraining-span]
    (let [increment 10 ;note that this must be true: (= 0 (mod constraining-span increment) (/ constraining-span 2))
          increment-numbers (range (inc (/ constraining-span increment)))
          base-angles (map #(- original-angle (- (* increment %) (/ constraining-span 2))) increment-numbers)
          angle-noise 2
          max-lengths 9]
      (for [angle (map #(+ % (rand-int (* angle-noise 2)) (- angle-noise)) base-angles)
            length (map #(* (inc %) road-length) (range max-lengths))]
        {:angle angle :length length}))))

(defn choose-angle
  "Given a point and possible offsets, returns the angle for the road"
  [p offsets]
  (:angle (reduce (fn [offset1 offset2]
                    (if (< (population-at (endpoint-for p (:angle offset1) (:length offset1)))
                           (population-at (endpoint-for p (:angle offset2) (:length offset2))))
                      offset2
                      offset1)) offsets)))

(defn new-road [point offsets & kvs]
  (let [opts (apply hash-map kvs)]
    (merge {:continuable? true :branchable? true :branch-delay 0}
           opts
           {:point point :angle (choose-angle point offsets)})))

(defn first-road
  "Creates an entirely new road with no predecessors"
  []
  (let [point [(round (rand-int size-x)) (round (rand-int size-y))]]
    (new-road point (offsets-to-check) :branch-delay 5)))

(defn new-continuation
  "Given a point and an original angle, will define a road starting at that position that is constrained by the original angle"
  [point original-angle & kvs]
  (let [span-in-degrees 60
        offsets (offsets-to-check original-angle span-in-degrees)
        opts (merge {:branch-delay 5} (apply hash-map kvs))]
    (apply new-road point offsets :branch-delay 5 kvs)))

(defn new-branch
  [road rotation]
  (new-continuation (endpoint-for road) (+ rotation (:angle road)) :branchable? false))

(defn next-roads
  "Given a single road, will return a seq of road(s) that take its place in the sequence"
  [road]
  (let [endpoint (endpoint-for road)]
    (cond
      (:continuable? road) [(assoc road :continuable? false) (new-continuation endpoint (:angle road))]
      (and (:branchable? road) (= 0 (:branch-delay road))) [(assoc road :branchable? false) (new-branch road -90) (new-branch road 90)]
      (:branchable? road) [(update-in road [:branch-delay] dec)]
      :else [road])))

(defn next-generation
  "Given a set of roads, returns the next generation of roads available"
  [roads]
  (if (empty? roads)
    [(first-road) (first-road) (first-road)]
    (flatten (map next-roads roads))))

(defn reset-roads
  "Resets a collection of roads to their initial state"
  ([] [])
  ([_] [])) ;for use with swap!

(defn mouse-pressed
  "Upon a mouse press, outputs the population at that coordinate. Used for inspection"
  []
  (let [point [(mouse-x) (mouse-y)]]
    (println (str "Population at (" point "): " (population-at point)))
    (println (str "Most populous angle:" (choose-angle point (offsets-to-check))))))

(def all-roads (atom (reset-roads)))

(defn draw-roads
  "Draws all roads for the current generation"
  [roads]
  (stroke 255 0 0 255)
  (stroke-weight 1)
  (dorun
    (for [road roads]
      (let [p1 (:point road)
            p2 (endpoint-for road)]
        (apply line (flatten [p1 p2]))))))

(defn key-pressed
  "When the up arrow is pressed, advance the sim and redraw. When the down arrow is pressed, reset the sim"
  []
  (let [keypress (key-as-keyword)]
    (if (some #{keypress} [:up :down])
      (do
        (println "Drawing...")
        (if (= keypress :up)
          (swap! all-roads next-generation)
          (swap! all-roads reset-roads))
        (draw-background)
        (draw-roads @all-roads)
        (println "Done!")))))

(defn launch
  "Starts the simulation"
  []
  (defsketch cloville-sketch :title "Cloville" :draw draw :setup setup :size [size-x size-y] :mouse-pressed mouse-pressed :key-pressed key-pressed))
