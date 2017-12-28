(ns clojure-brainfuck.core
  (:require [clojure.tools.logging :as log]
            [clojure.string :as string])
  (:gen-class))

(defn ctx->cell
  [ctx]
  (get-in ctx [:memory (:ptr ctx)]))

(defn ctx->update-cell
  [ctx fn]
  (update-in ctx [:memory (:ptr ctx)] fn))

(defn ctx->assoc-cell
  [ctx v]
  (ctx->update-cell ctx #(identity v)))

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
  (ctx->update-cell ctx inc))

(defn op-minus
  "Takes a context with :memory and :ptr, and decrements the value
  at :ptr"
  [ctx]
  (ctx->update-cell ctx dec))

(defn op-putchar
  "Print the current cell value as ASCII"
  [ctx]
  (print (char (ctx->cell ctx)))
  ctx)

(def op-dot op-putchar) ;; alias to reference the brainfuck symbol

(defn op-getchar
  "Store the first byte of the string read from STDIN at :ptr position"
  [ctx]
  (let [in-byte (first (.getBytes (read-line)))]
    (ctx->assoc-cell ctx in-byte)))

(def op-comma op-getchar) ;; alias to reference brainfuck symbol

(defn op-loop-end
  "Handle GOTO by either jumping to beginning if cell is not 0, or popping the
  stack if cell is 0"
  [pc stack ctx]
  (let [[head & tail] stack]
    (if (zero? (ctx->cell ctx))
      [(inc pc) tail ctx]
      [head tail ctx])))

(defn debug-println
  [s]
  (.println *err* s))

(def cells-per-line 40)

(defn pointer-line
  ([offset] (pointer-line offset cells-per-line))
  ([offset length] (string/join " " (assoc (vec (replicate cells-per-line " ")) offset "^"))))

(defn print-line-with-pointer
  [els ptr]
  (let [slices (partition-all cells-per-line els)
        curr-slice (nth slices (int (/ ptr cells-per-line)))
        curr-offset (mod ptr cells-per-line)]
    (debug-println (string/join " " curr-slice))
    (debug-println (pointer-line curr-offset))))

(defn print-ctx
  [ctx]
  (debug-println "Context:")
  (print-line-with-pointer (:memory ctx) (:ptr ctx)))

(defn print-prog-state
  [pc input]
  (debug-println "Program:")
  (print-line-with-pointer (string/split input #"") pc))

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
  [input & debug]
  (let [initial-context {:memory [0] :ptr 0}]
    (loop [pc 0 stack nil ctx initial-context]
      (if (>= pc (count input)) [pc stack ctx]
          (let [sym (get input pc)]
            (when debug
              (debug-println "Press Enter to continue")
              (read-line)
              (print-prog-state pc input)
              (print-ctx ctx))
            (cond
              (= sym loop-start) (recur (inc pc) (cons pc stack) ctx)
              (= sym loop-end) (let [[pc stack ctx] (op-loop-end pc stack ctx)]
                                 (recur pc stack ctx))
              :else (recur (inc pc) stack ((sym->op sym) ctx))))))))

(defn debug?
  [args]
  (= (first args) "-d"))

(defn -main
  [& args]
  (let [file-name (if (debug? args) (second args) (first args))]
    (let [input (slurp file-name)]
      (interpret input (debug? args)))))
