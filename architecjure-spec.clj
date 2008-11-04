(load-file "architecjure.clj")

(defn it-function []
  (it "should run example when called inside an describe"
    (=> (* 4 5 6) should = 120)))

(check-examples

(describe "describe, without examples" [])

(describe "describe, with examples" []
  (it "should run a single-line example"
    (=> (concat [1 2] [3 4]) should = [1 2 3 4]))

  (it "should run a multi-line example"
    (inc 2)
    (=> (inc 2) should = 3))

  (it "should run a multi-expectation example"
    (=> 1 should = 1)
    (=> 2 should = 2)))

(describe "describe, with examples in functions" []
  (it-function))

(describe "describe, with root level" [my-var 1] 
  (it "should run examples in the root level"
    (=> my-var should = 1))
  
  (describe "with nesting" []
    (it "should run examples in a nested level"
      (=> my-var should = 1))

    (describe "with deeper nesting" [my-var (inc my-var)]
      (it "should run examples in the deepest level"
	(=> my-var should = 2))))

  (describe "with separate nesting" []
    (it "should run separately nested examples"
      (=> my-var should = 1))))

)