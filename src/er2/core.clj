(ns er2.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def beats-temp 6)
(def frame-rate 30)
(def TAU (* 2 q/PI))
(def bpm 140)
(def ms-per-beat (/ 60000 bpm))
(def colors {:blue        [102 153 204]
             :yellow      [225 242 117]
             :orange      [225 140 66]
             :red         [225 60 56]
             :dark-red    [162 62 72]
             :alpha-gray1 [255 255 255 20]})

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
  (merge {:control ::a   :frame-rate 30} {}
         (create-circle-states ::a ::b ::c)))

(defn bjorklund
  ([n k on-v off-v]
   (let [full-gap (int (/ (- n k) k))
         overlap (mod (- n k) k)]
     (vec (-> (for [on (take k (repeat on-v))]
                (conj (take full-gap (repeat off-v)) on))
              (concat (take overlap (repeat off-v)))
              flatten))))
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

(defn update-circle [state control]
  (let [new-beat (mod (int (/ (q/millis) ms-per-beat))
                      (:n-beats state))
        new-n-beats (cond (key-control :up control)   (inc (:n-beats state))
                          (key-control :down control) (dec (:n-beats state))
                          :else                       (:n-beats state))
        new-n-hits (cond (key-control :right control) (min (inc (:n-hits state)) new-n-beats)
                         (key-control :left control)  (max (dec (:n-hits state)) 1)
                         :else                        (:n-hits state))
        rotation-target (* new-beat (/ TAU new-n-beats))]
    
    {:n-beats new-n-beats
     :prev-beat (:current-beat state)
     :current-beat new-beat 
     :rotation (next-rotation (:rotation state) rotation-target 0.05 0.4)
     :rotation-target rotation-target
     :n-hits new-n-hits
     :bjorklund (if (and (= new-n-hits (:n-hits state))
                         (= new-n-beats (:n-beats state)))
                  (:bjorklund state)
                  (bjorklund new-n-beats new-n-hits true false))}))

(defn update-state [state]
  (let [new-control (cond
                      (key-control :a) ::a
                      (key-control :b) ::b
                      (key-control :c) ::c
                      :else (:control state))]
    (assoc state
           ::a (update-circle (::a state) (= ::a new-control))
           ::b (update-circle (::b state) (= ::b new-control))
           ::c (update-circle (::c state) (= ::c new-control))
           :control new-control)))

(defn draw-circle [state rad col]
  (let [beat-arc (/ (* 1.5 q/PI) (:n-beats state)) ]
    (doseq [n (range (:n-beats state))]
      (q/with-rotation [(:rotation state)]
        (q/with-rotation [(* n (/ TAU (:n-beats state)))]
          (if (nth (:bjorklund state) n false) (apply q/fill col)
              (apply q/fill (:alpha-gray1 colors)))
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
    (q/text-align :center :center)
    (q/text (str bpm) 0 0)))

(q/defsketch er2
  :title ""
  :size [500 500]
  :setup setup
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])

