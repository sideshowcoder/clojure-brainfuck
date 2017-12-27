(ns clojure-brainfuck.core
  (:require [clojure.tools.logging :as log]
            [clojure.string :as string])
  (:gen-class))

(def str-to-op-map {\> op-greater-than
                    \< op-less-than
                    \+ op-plus
                    \- op-minus
                    \. op-dot
                    \, op-comma})

(def loop-start \[)
(def loop-end \])

(defn str->op
  [s]
  (get str-to-op-map s noop))

(defn interpret
  [input]
  (loop [pc 0
         stack nil
         ctx [0]
         idx 0]
    (if (>= pc (count input)) [pc stack ctx idx]
        (let [sym (get input pc)]
          (cond
            (= sym loop-start) (recur (inc pc) (cons pc stack) ctx idx)
            (= sym loop-end) (let [[pc stack ctx idx] (op-loop-end pc stack ctx idx)]
                               (recur pc stack ctx idx))
            :else (let [[ctx idx] ((str->op sym) ctx idx)]
                    (recur (inc pc) stack ctx idx)))))))

(defn noop
  [ctx idx]
  [ctx idx])

(defn op-greater-than
  [ctx idx]
  (let [new-idx (inc idx)
        new-ctx (update-in ctx [new-idx] #(or % 0))]
    [new-ctx new-idx]))

(defn op-less-than
  [ctx idx]
  (let [new-idx (if (> idx 0)
                  (do
                    (log/warn "ignore dec on index 0")
                    (dec idx)) idx)]
    [ctx new-idx]))

(defn op-plus
  [ctx idx]
  (let [new-ctx (update-in ctx [idx] inc)]
    [new-ctx idx]))

(defn op-minus
  [ctx idx]
  (let [new-ctx (update-in ctx [idx] dec)]
    [new-ctx idx]))

(defn op-putchar
  [ctx idx]
  (print (char (get ctx idx)))
  [ctx idx])

(def op-dot op-putchar)

(defn op-getchar
  [ctx idx]
  (let [in-byte (first (.getBytes (read-line)))
        new-ctx (assoc ctx idx in-byte)]
    [new-ctx idx]))

(def op-comma op-getchar)

(defn op-loop-end
  [pc stack ctx idx]
  (if (zero? (get ctx idx))
    (let [new-pc (inc pc)
          [_ & new-stack] stack]
      [new-pc new-stack ctx idx])
    (let [[new-pc & new-stack] stack]
      [new-pc new-stack ctx idx])))
