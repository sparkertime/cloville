(ns cloville.roads
  (:use quil.core))

(def LENGTH 10.0)

(defn endpoint-for
  ([x y angle] (endpoint-for x y angle LENGTH))
  ([x y angle length]
     [(round (+ x (* length (sin (radians angle)))))
      (round (+ y (* length (cos (radians angle)))))]))

(def dummy-road {:x 321 :y 420 :angle 91})

(def roads (atom [[320 400 321 420] [321 420 322 440] [322 440 323 460] [323 460 326 480] [326 480 333 500] [333 500 340 510]]))

(def branches [[321 420] [323 460]])

(defn local-constraints [road]
  "Checks the local constraints for a road and returns a new road that satisfies the local constraints, or null if none can be found. These local constraints include...
    * Checking roadway against water boundaries and truncating it up to a certain factor
    * Checking for intersections within a radius of the end segment and adjusting appropriately"
  [])

(defn new-roads []
  "Implementation of Global Contraints. Adds branches with delays in accordance to our global rules (attract to populations, follow global-rules (NY, Paris, SF, 'Basic'). Not entirely sure what this does yet" [])

(defn draw-roads []
  (stroke 255 0 0 255)
  (stroke-weight 2)
  (dorun
    (for [road @roads]
      (apply line road))))
