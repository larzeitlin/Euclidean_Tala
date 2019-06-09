(ns er2.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

; For some reason this stops quil from killing internal sc server
(q/defsketch dummy :size [10 10])
(quil.applet/applet-disposed dummy)
(use 'overtone.live)

(def beats-temp 6)
(def frame-rate 30)
(def TAU (* 2 q/PI))
(def bpm 220)
(def ms-per-beat (/ 60000 bpm))

;-----------------------> | R | G | B | A |
(def colors {:blue        [102 153 204 255]
             :yellow      [225 242 117 255]
             :orange      [225 140 66  255]
             :red         [225 60  56  255]
             :dark-red    [162 62  72  255]
             :alpha-gray1 [255 255 255 20 ]})

(def samples-ids
  {:tu          56153 ;1
   :ge          56146 ;1
   :xylophone   374699
   :berimbau    177068
   :hang-2      380509})

(def sound-a (freesound (:tu samples-ids)))
(def sound-b (freesound (:ge samples-ids)))
(def sound-c (freesound (:berimbau samples-ids)))

(defn create-circle-states [& names]
  (into {} (for [n names]
             [n {:n-beats         beats-temp
                 :prev-beat       0
                 :current-beat    0
                 :rotation        0
                 :rotation-target 0
                 :n-hits          1
                 :bjorklund       []}])))

(defn setup []
  (q/frame-rate frame-rate)
  (q/color-mode :rgb)
  (merge {:control        ::a
          :frame-rate     30
          :mode           ::poly
          :global-n-beats 8}
        
         (create-circle-states ::a ::b ::c)))

(defn bjorklund-kernel [v]
  (if (apply = v) (flatten v)
      (let [grouped (->> v (group-by identity) vals sort)
            group-small (first grouped)
            group-big (last grouped)
            overlap (->> group-big (split-at (count group-small)) second (map vector))
            output (concat (map vector group-big group-small) overlap)]
        (if (> 2 (count group-small)) (flatten output)
            (recur output)))))

(defn bjorklund
  ([n k on-v off-v]
   (cond (zero? k) (take n (repeat off-v))
         (= k n) (take n (repeat on-v))
         :else (let [initial-vector (concat (take k (repeat on-v))
                                            (take (- n k) (repeat off-v)))]
                 (bjorklund-kernel initial-vector))))
  ([n k]
   (bjorklund n k 1 0)))

(defn next-rotation [current-rotation rotation-target eps alp]
  (let [angle-diff (- rotation-target current-rotation)
        circle-difference (- (mod (+ angle-diff q/PI) TAU) q/PI)]
    (if (> eps (Math/abs circle-difference)) rotation-target
        (mod (+ current-rotation (* alp circle-difference)) TAU)))) ; Is this mod neccecery?

(defn key-control [keyword & bools]
  (and (q/key-pressed?)
       (= (q/key-as-keyword) keyword)
       (apply = true bools)))

(defn update-circle [state control sound mode new-global-n-beats]
  (let [n-beats (if (= mode ::locked) new-global-n-beats
                                      (:n-beats state))
        new-beat (mod (int (/ (q/millis) ms-per-beat)) n-beats)
        new-n-beats (cond (key-control :up control (= mode ::poly))   (inc n-beats)
                          (key-control :down control (= mode ::poly)) (max (dec n-beats) 1)
                          (= mode ::locked) new-global-n-beats
                          :else                       n-beats)
        new-n-hits (cond (key-control :right control) (min (inc (:n-hits state)) new-n-beats)
                         (key-control :left control)  (max (dec (:n-hits state)) 1)
                         :else                        (:n-hits state))
        rotation-target (* new-beat (/ TAU new-n-beats))]

    (when (and (not= new-beat (:current-beat state))
               (nth (:bjorklund state) new-beat false)) (sound))
    
    {:n-beats new-n-beats
     :prev-beat (:current-beat state)
     :current-beat new-beat 
     :rotation (next-rotation (:rotation state) rotation-target 0.05 0.4)
     :rotation-target rotation-target
     :n-hits new-n-hits
     :bjorklund (if (and (= new-n-hits (:n-hits state))
                         (= new-n-beats n-beats))
                  (:bjorklund state)
                  (bjorklund new-n-beats new-n-hits true false))}))

(defn update-state [state]
  (let [new-control (cond
                      (key-control :a) ::a
                      (key-control :b) ::b
                      (key-control :c) ::c
                      :else (:control state))
        new-mode (cond (key-control :p) ::poly
                       (key-control :l) ::locked
                       :else (:mode state))
        new-global-n-beats (cond (key-control :up (= ::locked (:mode state)))
                                 (inc (:global-n-beats state))
                                 
                                 (key-control :down (= ::locked (:mode state)))
                                 (max (dec (:global-n-beats state)) 1)
                                 
                                 :else
                                 (:global-n-beats state))]
    (assoc state
           ::a (update-circle (::a state) (= ::a new-control) sound-a (:mode state) new-global-n-beats)
           ::b (update-circle (::b state) (= ::b new-control) sound-b (:mode state) new-global-n-beats)
           ::c (update-circle (::c state) (= ::c new-control) sound-c (:mode state) new-global-n-beats)
           :control new-control
           :mode new-mode
           :global-n-beats new-global-n-beats)))

(defn draw-circle [state rad col]
  (let [beat-arc (/ (* 1.5 q/PI) (:n-beats state)) ]
    (doseq [n (range (:n-beats state))]
      (q/with-rotation [(:rotation state)]
        (q/with-rotation [(* n (/ TAU (:n-beats state)))]
          (cond
            (nth (:bjorklund state) (mod (inc (-  (dec (:n-beats state))
                                                  (mod n (:n-beats state))))
                                         (:n-beats state)) false)
            (apply q/fill col)
            
            :else (apply q/fill (:alpha-gray1 colors)))
          
          (q/arc 0 0 rad rad (* -0.5 beat-arc) (* 0.5 beat-arc) :pie)))))
  (q/fill 50)
  (q/stroke 50)
  (q/ellipse 0 0 (- rad 100) (- rad 100))) 

(defn draw-state [state]
  (q/background 50)
  (q/stroke 50)
  (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
    (q/with-rotation [(* -0.5 q/PI)]
      (q/fill 200)
      (q/ellipse 230 0 10 10)
      (draw-circle (::a state) 420 (:blue colors))
      (draw-circle (::b state) 300 (:red colors))
      (draw-circle (::c state) 180 (:yellow colors)))
    (q/fill 200)
    (q/text-size 20)
    (q/text-align :center :center)))

(q/defsketch er2
  :title ""
  :size [500 500]
  :setup setup
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
