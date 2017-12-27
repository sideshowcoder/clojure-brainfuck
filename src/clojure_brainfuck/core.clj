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

(defn sym->op
  [s]
  (get str-to-op-map s identity))

(defn interpret
  [input]
  (let [ctx {:memory [0] :ptr 0}])
  (loop [pc 0 stack nil ctx]
    (if (>= pc (count input)) [pc stack ctx]
        (let [sym (get input pc)]
          (cond
            (= sym loop-start) (recur (inc pc) (cons pc stack) ctx)
            (= sym loop-end) (let [[pc stack ctx idx] (op-loop-end pc stack ctx)]
                               (recur pc stack ctx))
            :else (let [[ctx idx] ((sym->op sym) ctx)]
                    (recur (inc pc) stack ctx)))))))

(defn op-greater-than
  "Take a context with :memory and :ptr and move the pointer to the
  right, initializing new fields along the way."
  [ctx]
  (let [ctx-new-ptr (update-in ctx [:ptr] inc)
        ctx-new-mem (update-in ctx-new-ptr [:memory (:ptr ctx-new-ptr)] #(or % 0))]
    ctx-new-mem))

(defn op-less-than
  "Takes a context with :memory and :ptr and moves the pointer to the left, only
  if :ptr is larger than 0"
  [ctx]
  (letfn [(safe-dec [x] (if (> x 0) (dec x) x))]
    (update-in ctx [:ptr] safe-dec)))

(defn op-plus
  "Takes a context with :memory and :ptr, and increments the value
  at :ptr"
  [ctx]
  (update-in ctx [:memory (:ptr ctx)] inc))

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
