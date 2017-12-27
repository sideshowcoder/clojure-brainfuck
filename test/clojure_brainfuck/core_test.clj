(ns clojure-brainfuck.core-test
  (:require [clojure.test :refer :all]
            [clojure.string :refer [split]]
            [clojure-brainfuck.core :refer :all]))

(deftest operator-greater-than
  (testing "> initializes new elements at new indexes."
    (is (= (op-greater-than [0] 0) [[0 0] 1])))
  (testing "> increments the index on already initialized elements."
    (is (= (op-greater-than [0 0] 0) [[0 0] 1]))))

(deftest operator-less-than
  (let [ctx (vec (range 3))]
    (testing "< on index 0 does nothing."
      (is (= (op-less-than ctx 0) [ctx 0])))
    (testing "< on index larger than 0 decrements the index."
      (is (= (op-less-than ctx 1) [ctx 0])))))

(deftest operator-plus
  (testing "+ increments at index."
    (is (= (op-plus [0] 0) [[1] 0]))))

(deftest operator-minus
  (testing "- decrements at index."
    (is (= (op-minus [1] 0) [[0] 0]))))

(deftest operator-putchar
  (testing ". prints the charactar at index"
    (let [out (with-out-str (op-dot [65] 0))]
      (is (= "A" out)))))

(deftest operator-getchar
  (testing ", reads a byte from the input storing at index in context."
    (let [res (with-in-str "ABC" (op-comma [] 0))]
      (is (= res [[65] 0])))))

(def basic-no-loop-prog  "++>+++")

(deftest no-loop-prog
  (testing "interpret a program without a loop."
    (let [[_ _ ctx _] (interpret basic-no-loop-prog)]
      (is (= ctx [2 3])))))

(deftest loop-test
  (testing "loop start pushes to stack."
    (let [[_ stack _ _] (interpret "[")]
      (is (= stack '(0)))))
  (testing "loop end pops stack."
    (let [[_ stack _ _] (interpret "[]")]
      (is (= stack nil)))))

(deftest op-loop-end-test
  (testing "inc pc past loop if cell is 0."
    (is (= (op-loop-end 0 '(0) [0] 0) [1 nil [0] 0])))
  (testing "it jumps to matching loop beginning on stack if cell is != 0"
    (is (= (op-loop-end 2 '(0) [1] 0) [0 nil [1] 0]))))

(def one-plus-two "+>++[<+>-]")

(deftest looping-program-test
  (testing "can add 2 numbers."
    (let [[_ _ ctx _] (interpret one-plus-two)]
      (is (= ctx [3 0])))))

(def hello-world
  "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.")

(deftest hello-world-test
  (testing "prints hello world."
    (is (= (with-out-str (interpret hello-world)) "Hello World!\n"))))
