(ns er2.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def beats-temp 6)

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb)
  {:angle        0
   :n-beats      beats-temp
   :prev-beat    0
   :current-beat 0
   :rotation     0})

; Need more smoothing

(defn next-rotation [current-rotation rotation-target eps alp]
  (let [angle-diff (- rotation-target current-rotation)
        circle-difference (- (mod (+ angle-diff q/PI) (* 2 q/PI)) q/PI)]
    (if (> eps (Math/abs circle-difference)) rotation-target
        (mod (+ (* 2 q/PI) current-rotation (* alp circle-difference))
             (* 2 q/PI)))))

(defn update-state [state]
  (let [new-beat (mod (int (/ (q/frame-count)
                              (q/current-frame-rate)))
                      (:n-beats state))
        new-n-beats (cond (and (q/key-pressed?) (= (q/key-as-keyword) :up)) (inc (:n-beats state))
                          (and (q/key-pressed?) (= (q/key-as-keyword) :down)) (dec (:n-beats state))
                          :else (:n-beats state))
        rotation-target (* new-beat (/ (* q/PI 2) new-n-beats))]
    {:angle   (:angle state) 
     :n-beats new-n-beats
     :prev-beat (:current-beat state)
     :current-beat new-beat
     :rotation (next-rotation (:rotation state) rotation-target 0.05 0.4)
     :rotation-target rotation-target}))

(defn draw-state [state]
  (q/background 240)
  (q/stroke 240)
  (q/with-translation [(/ (q/width) 2)
                       (/ (q/height) 2)]
    (doseq [n (range (:n-beats state))]
      (q/with-rotation [(:rotation state)]
        (q/with-rotation [(* n (/ (* 2 q/PI) (:n-beats state)))]
          (if (= n 0) (q/fill 255 255 255) (q/fill 0 0 0))
          (q/arc 0 0 300 300 0 (/ (* 1 q/PI) (:n-beats state)) :pie))))
    (q/fill 240)
    (q/stroke 240)
    (q/ellipse 0 0 200 200))

  ; feed 
  (q/fill 0)
  (q/text (str (:rotation-target state) " : " (q/key-as-keyword)) 10 10))


(q/defsketch er2
  :title ""
  :size [500 500]
  :setup setup
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])

