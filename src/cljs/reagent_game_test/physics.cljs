(ns reagent-game-test.physics
  (:require [Matter]))

(enable-console-print!)

(def rectangle js/Matter.Bodies.rectangle)
(def circle js/Matter.Bodies.circle)
(def add js/Matter.World.add)
(def remove js/Matter.World.remove)
(def apply-force js/Matter.Body.applyForce)

(defn make-renderer [update-callback]
  (def renderer {:create (fn [options]
                           (print "renderer.create")
                           (clj->js {:controller renderer}))
                 :world update-callback})
  renderer)

(defn make-physics-engine [update-callback collision-callback]
  (let [engine (js/Matter.Engine.create (clj->js {:render {:controller (make-renderer update-callback)}}))]
    (js/Matter.Events.on engine "collisionStart" collision-callback)
    engine))

(defn run [engine]
      (print "launched")
      (js/Matter.Engine.run engine)
  engine)

(defn apply-impulse [engine id dx dy]
  (doall (for [b engine.world.bodies]
    (if (= b.id (js/parseInt (.substr id 8)))
      (js/Matter.Body.applyForce b (clj->js {:x 0 :y 0}) (clj->js {:x dx :y dy}))))))
