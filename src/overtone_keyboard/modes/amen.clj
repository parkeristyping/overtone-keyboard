(ns overtone-keyboard.modes.amen
  (:use [overtone.live]))

(defn amen
  [n]
  (if (> n 0)
    ((sample (str "resources/samples/amen/"
                  (format "%02d" n)
                  ".wav")))))

(def max-length 18)

(def state
  (atom {:active? false
         :bpm 120
         :length 8
         :seqx (vec (repeat max-length 1))
         :offset 0}))

(defn amen-step
  ([] (amen-step 0))
  ([x]
   (let [{:keys [seqx active? length bpm]} @state]
     (amen (get seqx x))
     (if active?
       (apply-by (+ (/ 12000 bpm) (now))
                 amen-step
                 (mod (inc x) length)
                 [])))))

(defn start-amen!
  []
  (swap! state assoc :active? true)
  (amen-step 0))

(defn stop-amen!
  []
  (swap! state assoc :active? false))

(def sliders [74 71 91 93 73 72 5 84 7])

(defn midi->amen
  [midi-value]
  (- 32 (int (Math/floor (* 32 (/ midi-value 127))))))

(defn midi->bpm
  [midi-value]
  midi-value)

(defn midi->length
  [midi-value]
  (inc (int (Math/floor (* (- max-length 1) (/ midi-value 127))))))

(defn initialize-handlers
  []
  ;; change offset
  (on-event [:midi :program-change]
            (fn [e]
              (let [note (:note e)]
                (swap! state assoc :offset note)))
            ::offset)
  ;; change samples
  (on-event [:midi :control-change]
            (fn [e]
              (let [{:keys [note velocity]} e
                    {:keys [offset]} @state
                    idx (.indexOf sliders note)]
                (if (>= idx 0)
                  (swap!
                   state
                   assoc-in
                   [:seqx (+ idx (* 9 offset))]
                   (midi->amen velocity)))))
            ::amen-samples)
  ;; change length and tempo
  (on-event [:midi :control-change]
            (fn [e]
              (let [{:keys [note velocity]} e]
                (cond (= note 75)
                      (swap! state assoc :bpm (midi->bpm velocity))
                      (= note 76)
                      (swap! state assoc :length (midi->length velocity)))))
            ::length-and-tempo)
  ;; stop
  (on-event [:midi :note-on]
            (fn [e]
              (let [channel (:channel e)
                    note (:note e)]
                (if (and (= 9 channel)
                         (= 50 note))
                  (stop-amen!))))
            ::killswitch))

(defn activate
  []
  (initialize-handlers)
  (start-amen!))

(defn de-activate
  []
  (remove-event-handler ::offset)
  (remove-event-handler ::amen-samples)
  (remove-event-handler ::length-and-tempo)
  (remove-event-handler ::killswitch))

;; (activate)
;; (initialize-handlers)
;; (de-activate)
;; (deref state)
;; (start-amen!)
;; (stop-amen!)
