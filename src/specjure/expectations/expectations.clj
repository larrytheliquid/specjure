(ns specjure)
(defstruct expectation :comparator :actual :expected)

(defn passed? [expectation]
  ((:comparator expectation) (:expected expectation) (:actual expectation)))

(defn parse-matcher [matcher & arguments]
  (cond (= matcher '=) arguments
	(= matcher 'be-true) [true (not (not (first arguments)))]
	(= matcher 'be-false) [false (not (not (first arguments)))]))

