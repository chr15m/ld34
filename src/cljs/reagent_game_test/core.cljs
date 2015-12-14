(ns reagent-game-test.core
    (:require [reagent-game-test.sfx :as sfx]
              [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [goog.events :as events]
              [goog.dom :as dom]
              [goog.history.EventType :as EventType]
              [cljs-uuid-utils.core :as uuid]
              [cljs.core.async :refer [chan <! timeout]]
              [reagent-game-test.physics :as physics])
    (:require-macros [cljs.core.async.macros :refer [go go-loop]])
    (:import goog.History))

(enable-console-print!)

; all of the entities that appear in our game
(def game-state (atom {:entities {}}))
(defonce delete-physics-entities (atom {}))
(def drag (atom nil))
(defonce physics-engine (atom nil))
(defonce viewport-size (atom {}))
(defonce hurt-count (atom 20))
(defonce won (atom 1.0))

(def blurb "ld34.")
(def physics-scale 1000.0)

(def bad-things ["☢" "⚔" "☠" "⚡"])

(print blurb)

(print "initial game-state: " (map (fn [[id e]] (print id "->" e)) (:entities @game-state)))

;; -------------------------
;; Helper functions

(defn rnd [] (js/Math.random))
(defn rnd-int [a b] (js/Math.round (+ (* (js/Math.random) (- b a)) a)))

; handle window resizing
(defn re-calculate-viewport-size [old-viewport-size]
  (let [viewport-size (dom/getViewportSize (dom/getWindow))
        w (.-width viewport-size)
        h (.-height viewport-size)]
    {:w w
     :h h
     :extent (/ (min w h) 2.0)
     :ratio (/ (min w h) 1024.0)
     :scaled-width (/ w (min w h))
     :scaled-height (/ h (min w h))}))

(defn translate-screen-coordinates [[x y]]
  (let [{:keys [w h extent ratio]} @viewport-size]
    [(/ (- x (/ w 2)) extent)
     (/ (- y (/ h 2)) extent)]))

(defn get-time-now [] (.getTime (js/Date.)))

(defn get-drag-values [ev]
  (let [[x y] (translate-screen-coordinates [ev.clientX ev.clientY])
        [dx dy] [(- ((:source-point @drag) 0) x) (- ((:source-point @drag) 1) y)]
        pos [(+ (/ dx 2.0) x) (+ (/ dy 2.0) y)]
        angle (js/Math.atan2 dy dx)
        distance (js/Math.sqrt (+ (js/Math.pow dx 2) (js/Math.pow dy 2)))]
    [dx dy pos angle distance]))

(defn drag-start [ev]
  (reset! drag {:source-point (translate-screen-coordinates [ev.target.offsetLeft ev.target.offsetTop]) :source-id (.getAttribute ev.target "id")}))

(defn drag-update [ev]
  (let [[dx dy pos angle distance] (get-drag-values ev)]
    (swap! game-state assoc-in [:entities :dragger] {:id :dragger :pos pos :symbol "" :class "solid" :color 4 :size [distance 0.01] :angle (/ angle (* Math.PI 2)) :style {:z-index 1000}})))

(defn drag-end [ev]
  (when @drag
    (sfx/play :blip)
    ; apply impulse
    (let [[dx dy pos angle distance] (get-drag-values ev)]
      (physics/apply-impulse @physics-engine (:source-id @drag) (* dx physics-scale -0.004) (* dy physics-scale -0.004)))
    (swap! game-state update-in [:entities] dissoc :dragger)
    (reset! drag nil)))

; turn a position into a CSS style declaration
(defn compute-position-style [{[x y] :pos angle :angle [ew eh] :size :or {angle 0}}]
  (let [{:keys [w h extent ratio]} @viewport-size
        position-style {:left (+ (* x extent) (/ w 2))
                        :top (+ (* y extent) (/ h 2))
                        :transform (str "translate(-50%, -50%) rotate(" angle "turn) scale(" ratio ", " ratio ")")}]
    (if ew
      (assoc position-style :width (* ew (/ extent ratio)) :height (* eh (/ extent ratio)))
      position-style)))


(defn play-random-sound [sound-type]
  (let [sfx-name (str sound-type "-" (+ (js/Math.round (* (js/Math.random) 2)) 1))]
    (sfx/play (keyword sfx-name))))   

(defn behaviour-static [old-state elapsed now]
  old-state)

(defn behaviour-loop [old-state elapsed now]
  (assoc old-state :pos [(* (Math.cos (/ now 500)) 0.200)
                         (* (Math.sin (/ now 500)) 0.200)]))

(defn behaviour-rock [old-state elapsed now]
  (-> old-state
      (assoc-in [:pos 0] (* (Math.cos (/ now 500)) 0.05))
      (assoc-in [:angle] (Math.cos (/ now 2000)))))

(defn make-box [p1 p2 s1 s2 & [options entity]]
  (let [original-pos [p1 p2 s1 s2]
        pos (vec (map #(* % physics-scale) original-pos))
        ; pos original-pos
        new-options (clj->js (merge options {:renderInfo {:originalSize [s1 s2]}}))]
    (set! (.-entity new-options) entity)
    (apply physics/rectangle (conj pos new-options))))

(defn make-star [p1 p2 & [options entity]]
  (play-random-sound "coin")
  (let [r 0.075
        original-pos [p1 p2 r]
        pos (vec (map #(* % physics-scale) original-pos))
        ; pos original-pos
        new-options (clj->js (merge options {:renderInfo {:originalSize [(* r 2) (* r 2)] :label "Star"}}))
        new-entity (merge entity {:symbol "★" :color 2 :style {:font-size "5em"} :class ""})]
    (set! (.-entity new-options) new-entity)
    (let [new-star (apply physics/circle (conj pos new-options))]
      (go (<! (timeout 2000))
          (swap! game-state update-in [:entities] dissoc (str "physics-" (.-id new-star)))
          (physics/remove (.-world @physics-engine) new-star))
      (go (<! (timeout 10))
          (physics/apply-impulse @physics-engine (str "physics-" (.-id new-star)) (* 2 (- (rnd) 0.5)) (* 2 (- (rnd) 0.9))))
      new-star)))

(defn make-stars [c o d]
  (go-loop [counter c]
           (let [x (/ (aget (.-position o) "x") physics-scale) y (/ (aget (.-position o) "y") physics-scale)]
             (physics/add (.-world @physics-engine) (clj->js [(make-star x y)]))
             (<! (timeout d))
             (when (> counter 0)
               (recur (- counter 1))))))

(defn engine-updated [engine]
  ; (print "renderer.world")
  ; (js/console.log engine.world.bodies)  
  (doall (for [b engine.world.bodies]
           ; (js/console.log b)
           (let [game-id (str "physics-" b.id)
                 extent (:extent @viewport-size)
                 half-extent (/ extent 2)]
             ;(print game-id)
             ;(print b.position)
             (swap! game-state assoc-in
                    [:entities game-id]
                    (merge {:id game-id
                            :symbol ""
                            :color 0
                            :class "outline"
                            :size [(aget b.renderInfo.originalSize 0) (aget b.renderInfo.originalSize 1)]
                            :pos [(/ (aget b.position "x") physics-scale) (/ (aget b.position "y") physics-scale)]
                            :angle (/ b.angle (* Math.PI 2))} b.entity)))))
  ; remove physics entities that have been deleted
  (doseq [[id o] @delete-physics-entities]
    (swap! game-state update-in [:entities] dissoc id)
    (physics/remove (.-world @physics-engine) o))
  ; subtract number removed from hurt-count
  (swap! hurt-count (fn [old-hurt-count]
                      (let [new-hurt-count (- old-hurt-count (count @delete-physics-entities))]
                        ; if we've hit zero the first time, add the goal box
                        (if (and (> old-hurt-count 0) (<= new-hurt-count 0))
                          (go
                            (<! (timeout 2000))
                            (physics/add engine.world (clj->js [(make-box 0 -0.5 0.2 0.2
                                                                          {:label "Luv" :isStatic true}
                                                                          {:symbol "❤" :style {:font-size "5.0em"} :color 3})]))))
                        new-hurt-count)))
  ; reset delete list
  (reset! delete-physics-entities {}))

(defn engine-collision [ev]
  (doseq [p ev.pairs]
    (do
      (doseq [[b o] [[p.bodyA p.bodyB] [p.bodyB p.bodyA]]]
        (when
          (= (.-label b) "Player")
          (when (= (.-label o) "Block")
            (play-random-sound "bump")
            (make-stars 3 o 10)
            ; add to delete list
            (swap! delete-physics-entities assoc-in [(str "physics-" (.-id o))] o)
            ; hitting a block grows your heart
            (set! (.-entity b) (-> (.-entity b)
                                   (update-in [:heart-size] #(+ % 3))
                                   (assoc-in [:style :font-size] (str (.toFixed (/ (get-in (.-entity b) [:heart-size]) 10) 2) "em")))))
          (when (= (.-label o) "Luv")
            (if (= @won 1.0)
              (make-stars 100 b 50))
            (go-loop []
                     (<! (timeout 20))
                     (swap! won #(js/Math.max (- % 0.005) 0))   
                     (if (> @won 0)
                       (recur))))
          (when (= (.-label o) "Ground")
            (play-random-sound "bump")))))))

; insert a single new entity record into the game state and kick off its control loop
; entity-definition = :symbol :color :pos :angle :behaviour
(defn make-entity [entity-definition]
  (let [id (uuid/uuid-string (uuid/make-random-uuid))
        entity {id (assoc entity-definition :id id :chan (chan))}]
    ; swap the new entity definition into our game state
    (swap! game-state assoc-in
      [:entities id] (entity id))
    ; kick off the entity's control loop
    (go-loop [last-time (get-time-now)]
      ; run every 20 milliseconds
      (<! (timeout 20))
      (let [now (get-time-now)
            elapsed (- now last-time)]
        ; update this entity's properties according to its behaviour function
        (let [behaviour-fn (get-in @game-state [:entities id :behaviour])]
          (if (not (nil? behaviour-fn))
            (swap! game-state update-in [:entities id] behaviour-fn elapsed now)))
        (recur now)))
    ; return the entity we created
    entity))

;; -------------------------
;; Views

(defn home-page []
  [:div
    [:div {:id "game-board"}
        ; DOM "scene grapher"
        (doall (map
                 (fn [[id e]] [:div (merge {:class (str "sprite c" (:color e) " " (:class e)) :key id :id id :style (merge (compute-position-style e) (:style e))} (:entity-args e)) (:symbol e)])
                 (:entities @game-state)))
        (if (< @won 1.0) [:div {:id "fade" :style {:opacity (- 1.0 @won)}}])]
    ; info blurb
    (if (<= @won 0)
      [:div {:class "info-container"}
        [:div {:class "info c3"} blurb " [ " [:a {:href "http://github.com/chr15m/ld34"} "source code"] " ]"]
        [:div {:class "info smaller c3"} [:p "it is only with the heart that one can see rightly"] [:p "what is essential is invisible to the eye"] [:p "-- the little prince"]]])
    ; tv scan-line effect
    [:div {:id "overlay"}]])

;; -------------------------
;; Initialize app

; get the real viewport size for the first time
(swap! viewport-size re-calculate-viewport-size)

(defonce listeners (do
  ; update the current viewport size if it changes
  (js/window.addEventListener "resize" #(swap! viewport-size re-calculate-viewport-size))

  ; ignore all mouse downs
  (js/window.addEventListener "mousedown" (fn [ev] (.preventDefault ev)))

  ; when the mouse is lifted, null out the drag
  (js/window.addEventListener "mouseup" (fn [ev] (drag-end ev) (.preventDefault ev)))
  (js/window.addEventListener "touchend" (fn [ev] (drag-end ev) (.preventDefault ev)))

  ; when the mouse is moved, update the drag
  (js/window.addEventListener "mousemove" (fn [ev] (if @drag (drag-update ev)) (.preventDefault ev)))))

(defonce engine
  (let [engine (physics/make-physics-engine #(engine-updated %) #(engine-collision %))
        width (:scaled-width @viewport-size)
        height (:scaled-height @viewport-size)
        width-doubled (* 2 (:scaled-width @viewport-size))
        height-doubled (* 2 (:scaled-height @viewport-size))]
    (print "creating physics engine")
    ; add the player
    (physics/add engine.world (clj->js [(make-box -0.2 0.2 0.2 0.2 {:label "Player"} {:symbol "❤"
                                                                                      :heart-size 5
                                                                                      :style {:font-size "0.5em"}
                                                                                      :color 4
                                                                                      :entity-args {:on-click #(sfx/play :blip)
                                                                                                    :on-mouse-down drag-start
                                                                                                    :on-touch-start drag-start}})]))
    (doseq [x (range @hurt-count)]
      (physics/add engine.world (clj->js [(make-box (* (- (rnd) 0.5) width 1.6)  (* (- (rnd) 0.5) height 1.6) 0.2 0.2 {:label "Block" :isStatic true} {:symbol (get bad-things (rnd-int 0 3)) :color (rnd-int 0 2)})])))
    (physics/add engine.world (clj->js [(make-star 0 0)]))
    ; add walls
    (physics/add engine.world (clj->js [(make-box 0 1.0 width-doubled 0.05 {:isStatic true :label "Ground"} {:class "solid"})
                                        (make-box 0 -0.95 width-doubled 0.05 {:isStatic true :label "Ground"} {:style {:display "none"}})
                                        (make-box (* width -1.0) 0 0.05 height-doubled {:isStatic true :label "Ground"} {:style {:display "none"}})    
                                        (make-box width 0 0.05 height-doubled {:isStatic true :label "Ground"} {:style {:display "none"}})])) 
    (reset! physics-engine engine)
    (physics/run engine)))

; make the world move faster
(set! (.-timeScale (.-timing @physics-engine)) 1.5)

(defn mount-root []
  (reagent/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
