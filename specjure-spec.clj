(load-file "specjure.clj")

(defn it-function []
  (it "should run example when called inside a describe"
    (should = (* 4 5 6) 120)))

(check-examples

(describe "describe, without examples" [])

(describe "describe, with examples" []
  (it "should run a single-line example"
    (should = (concat [1 2] [3 4]) [1 2 3 4]))

  (it "should run a multi-line example"
    (inc 2)
    (should = (inc 2) 3))

  (it "should run a multi-expectation example"
    (should = 1 1)
    (should = 2 2)))

(describe "describe, with examples in functions" []
  (it-function))

(describe "describe, with root level" [my-var 1] 
  (it "should run examples in the root level"
    (should = my-var 1))
  
  (describe "with nesting" []
    (it "should run examples in a nested level"
      (should = my-var 1))

    (describe "with deeper nesting" [my-var (inc my-var)]
      (it "should run examples in the deepest level"
	(should = my-var 2))))

  (describe "with separate nesting" []
    (it "should run separately nested examples"
      (should = my-var 1))))

(describe "should, with the = matcher" []
  (describe "with equal arguments" []
    (it "should be true"
      (should = 3 3))))

(describe "should, with the be-false matcher" []
  (describe "with false" []
    (it "should be true"
      (should be-false false))))

(describe "should, with the be-true matcher" []
  (describe "with true" []
    (it "should be true"
      (should be-true true))))

(describe "should, with a be-predicate? matcher" []
  (describe "with pos? and a positive number" []
    (it "should be true"
      (should be-pos 3))))

(describe "should-not, with the = matcher" []
  (describe "with unequal arguments" []
    (it "should be true"
      (should-not = 3 5))))

(describe "should-not, with the be-false matcher" []
  (describe "with true" []
    (it "should be true"
      (should-not be-false true))))

(describe "should-not, with the be-true matcher" []
  (describe "with false" []
    (it "should be true"
      (should-not be-true false))))

(describe "should-not, with a be-predicate? matcher" []
  (describe "with pos? and a negative number" []
    (it "should be true"
      (should-not be-pos -3))))

)