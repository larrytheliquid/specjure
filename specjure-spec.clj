(ns specjure)

(defn it-function []
  (it "runs examples when called inside a describe"
    (should = (* 4 5 6) 120)))

(describe "specjure/describe without examples")

(describe "specjure/describe with examples"
  (it "runs a single-line example"
    (should = (concat [1 2] [3 4]) [1 2 3 4]))

  (it "runs a multi-line example"
    (inc 2)
    (should = (inc 2) 3))

  (it "runs a multi-expectation example"
    (should = 1 1)
    (should = 2 2)))

(describe "specjure/describe with examples in functions"
  (it-function))

(describe-let "specjure/describe-let with root level" [my-var 1] 
  (it "runs examples in the root level"
    (should = my-var 1))
  
  (describe "with nesting"
    (it "runs examples in a nested level"
      (should = my-var 1))

    (describe-let "with deeper nesting" [my-var (inc my-var)]
      (it "runs examples in the deepest level"
	(should = my-var 2))))

  (describe "with separate nesting"
    (it "runs separately nested examples"
      (should = my-var 1))))

(describe "specjure/should with the = matcher"
  (describe "with equal arguments"
    (it "returns true"
      (should = 3 3))))

(describe "specjure/should with the be-false matcher"
  (describe "with false"
    (it "returns true"
      (should be-false false))))

(describe "specjure/should with the be-true matcher"
  (describe "with true"
    (it "returns true"
      (should be-true true))))

(describe "specjure/should with a be-predicate matcher"
  (describe "with pos? and a positive number"
    (it "returns true"
      (should be-pos 3))))

(describe "specjure/should-not with the = matcher"
  (describe "with unequal arguments"
    (it "returns true"
      (should-not = 3 5))))

(describe "specjure/should-not with the be-false matcher"
  (describe "with true"
    (it "returns true"
      (should-not be-false true))))

(describe "specjure/should-not with the be-true matcher"
  (describe "with false"
    (it "returns true"
      (should-not be-true false))))

(describe "specjure/should-not with a be-predicate matcher"
  (describe "with pos? and a negative number"
    (it "returns true"
      (should-not be-pos -3))))