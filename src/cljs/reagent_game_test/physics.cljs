(ns reagent-game-test.physics
  (:require [Matter]))

(enable-console-print!)

(def rectangle js/Matter.Bodies.rectangle)
(def add js/Matter.World.add)

(defn make-renderer [update-callback]
  (def renderer {:create (fn [options]
                           (print "renderer.create")
                           (clj->js {:controller renderer}))
                 :world (fn [engine] (update-callback engine))})
  renderer)

(defn make-physics-engine [update-callback]
  (let [engine (js/Matter.Engine.create (clj->js {:render {:controller (make-renderer update-callback)}}))]
    (js/console.log "bodies" engine.world.bodies)
    engine))

(defn run [engine]
      (print "launched")
      (js/Matter.Engine.run engine)
  engine)

