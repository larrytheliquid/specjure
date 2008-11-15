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

)