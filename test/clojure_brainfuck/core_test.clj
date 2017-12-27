(ns clojure-brainfuck.core-test
  (:require [clojure.test :refer :all]
            [clojure.string :refer [split]]
            [clojure-brainfuck.core :refer :all]))

(deftest operator-greater-than
  (testing "> initializes new elements at new indexes."
    (is (= (op-greater-than {:memory [0] :ptr 0}) {:memory [0 0] :ptr 1})))
  (testing "> increments the index on already initialized elements."
    (is (= (op-greater-than {:memory [0 0] :ptr 0}) {:memory [0 0] :ptr 1}))))

(deftest operator-less-than
  (let [ctx (vec (range 3))]
    (testing "< on index 0 does nothing."
      (is (= (op-less-than {:memory [0] :ptr 0}) {:memory [0] :ptr 0})))
    (testing "< on index larger than 0 decrements the index."
      (is (= (op-less-than {:memory [0 0] :ptr 1}) {:memory [0 0] :ptr 0})))))

(deftest operator-plus
  (testing "+ increments at index."
    (is (= (op-plus {:memory [0] :ptr 0}) {:memory [1] :ptr 0}))))

(deftest operator-minus
  (testing "- decrements at index."
    (is (= (op-minus {:memory [1] :ptr 0}) {:memory [0] :ptr 0}))))

(deftest operator-putchar
  (testing ". prints the charactar at index"
    (is (= "A" (with-out-str (op-dot {:memory [65] :ptr 0}))))))

(deftest operator-getchar
  (testing ", reads a byte from the input storing at index in context."
    (is (= (with-in-str "ABC" (op-comma {:memory [0] :ptr 0})) {:memory [65] :ptr 0}))))

(def basic-no-loop-prog  "++>+++")

(deftest no-loop-prog
  (testing "interpret a program without a loop."
    (let [[_ _ ctx] (interpret basic-no-loop-prog)]
      (is (= {:memory [2 3] :ptr 1})))))

(deftest loop-test
  (testing "loop start pushes to stack."
    (let [[_ stack _ _] (interpret "[")]
      (is (= stack '(0)))))
  (testing "loop end pops stack."
    (let [[_ stack _ _] (interpret "[]")]
      (is (= stack nil)))))

(deftest op-loop-end-test
  (testing "inc pc past loop if cell is 0."
    (is (= (op-loop-end 0 '(0) {:memory [0] :ptr 0}) [1 nil {:memory [0] :ptr 0}])))
  (testing "it jumps to matching loop beginning on stack if cell is != 0"
    (is (= (op-loop-end 2 '(0) {:memory [1] :ptr 0}) [0 nil {:memory [1] :ptr 0}]))))

(def one-plus-two "+>++[<+>-]")

(deftest looping-program-test
  (testing "can add 2 numbers."
    (let [[_ _ ctx] (interpret one-plus-two)]
      (is (= ctx {:memory [3 0] :ptr 1})))))

(def hello-world
  "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.")

(deftest hello-world-test
  (testing "prints hello world."
    (is (= (with-out-str (interpret hello-world)) "Hello World!\n"))))
