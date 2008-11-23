(ns specjure)
(defstruct expectation :comparator :actual :expected)

(defn passed? [expectation]
  ((:comparator expectation) (:expected expectation) (:actual expectation)))

(defn be-function [be-matcher]
  (resolve (symbol (apply str 
			  (conj (vec (drop 3 (str be-matcher))) \?)))))

(defn parse-matcher [matcher & arguments]
  (cond (= matcher '=) arguments
	(= (symbol (apply str (take 3 (str matcher))))  'be-) 
	[(apply (be-function matcher)  arguments) true]))

