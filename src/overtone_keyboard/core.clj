(ns overtone-keyboard.core
  (:use [overtone.live]
        [overtone.inst.synth])
  (:require [clojure.string :as str])
  (:gen-class))

;; Functions for checking out midi stuff
(midi-connected-devices)
(event-debug-on)
(event-debug-off)


;; base instrument
(definst beep [note 60]
  (let [sound-src (sin-osc (midicps note))
        env       (env-gen (perc 0.005 1) :action FREE)]
    (* sound-src env)))

;; the repeater
(defn repeater [note-active? delay note]
  (if (note-active? note)
    (do
      (beep note)
      (apply-by (+ (delay) (now)) repeater note-active? delay note []))))

(def printer
  (agent nil))

(defn print-status
  [seqq step]
  (let [[x y] (split-at step seqq)
        a (map #(str " " % " ") x)
        b (str "[" (first y) "]")
        c (map #(str " " % " ") (rest y))]
    (send-off printer (fn [x] (println "\r" (str/join (flatten [a b c]))) nil))))

;; the arpeggiator
(defn seqquencer [note-active? seqq delayy note step]
  (let [current-seqq (seqq)]
    (if (note-active? note)
      (do
        (print-status current-seqq step)
        (beep (+ note (or (get current-seqq step) 0)))
        (apply-by (+ (delayy) (now))
                  seqquencer
                  note-active?
                  seqq
                  delayy
                  note
                  (mod (inc step) (max (count current-seqq) 1))
                  [])))))

;; the keyboard controls (m-audio axiom)
(def controls
  (atom
   {
    ;; knobs top row
    75 1
    76 1
    92 1
    95 1
    ;; knobs bottom row
    10 1
    77 1
    78 1
    79 1
    ;; sliders
    74 1
    71 1
    91 1
    93 1
    73 1
    72 1
    5  1
    84 1
    7  1}))

;; setup some functions that consume the controls
(defn scale-midi-to
  [range midi-value]
  (int (Math/floor (* range (/ midi-value 128)))))

(defn delayy
  []
  (let [c @controls]
    (* (get c 75)
       (get c 76))))

(defn seqq-len
  []
  (inc (scale-midi-to 8 (get @controls 10))))

(defn seqq
  []
  (let [c @controls
        full-seqq (vals (select-keys c [74 71 91 93 73 72 5 84 7]))
        scaled-seqq (map #(scale-midi-to 12 %) full-seqq)]
    (vec (take (seqq-len) scaled-seqq))))

;; link controls up with the midi keyboard
;; and print helpful outputs
(defn init-controls
  []
  (on-event [:midi :control-change]
            #(let [control (:note %)
                   value (:velocity %)]
               (swap! controls assoc control value))
            ::control-updates))

;; some atoms for our repeater
(def held-notes
  (atom #{}))

(defn note-active?
  [note]
  (contains? @held-notes note))

(defn init-keys
  []
  (on-event [:midi :note-on]
            (fn [e]
              (let [note (:note e)
                    vel  (:velocity e)]
                (swap! held-notes #(set (conj % note)))
                (seqquencer note-active? seqq delayy note 0)))
            ::keyboard-handler-on)
  (on-event [:midi :note-off]
            (fn [e]
              (let [note (:note e)]
                (swap! held-notes #(set (remove #{note} %)))))
            ::keyboard-handler-off))

(defn -main
  []
  (init-controls)
  (init-keys))
