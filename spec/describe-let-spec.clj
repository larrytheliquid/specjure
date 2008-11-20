(ns specjure)

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