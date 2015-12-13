(ns reagent-game-test.physics
  (:require [Matter]))

(enable-console-print!)

(def rectangle js/Matter.Bodies.rectangle)
(def add js/Matter.World.add)

(defn make-renderer [update-callback]
  (def renderer {:create (fn [options]
                           (print "renderer.create")
                           (clj->js {:controller renderer}))
                 :world update-callback})
  renderer)

(defn make-physics-engine [update-callback]
  (let [engine (js/Matter.Engine.create (clj->js {:render {:controller (make-renderer update-callback)}}))]
    engine))

(defn run [engine]
      (print "launched")
      (js/Matter.Engine.run engine)
  engine)

(defn apply-impulse [engine id dx dy]
  (doall (for [b engine.world.bodies]
    (if (= b.id (js/parseInt (.substr id 8)))
      (js/Matter.Body.applyForce b #js {:x 0 :y 0} #js {:x dx :y dy})))))
