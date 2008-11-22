(ns specjure)

(desc desc "with root level"
  (:before [my-var 1]) 

  (it "runs examples in the root level"
    (should = my-var 1))
  
  (desc "with nesting" ()
    (it "runs examples in a nested level"
      (should = my-var 1))

    (desc "with deeper nesting" 
      (:before [my-var (inc my-var)])

      (it "runs examples in the deepest level"
	(should = my-var 2))))

  (desc "with separate nesting" ()
    (it "runs separately nested examples"
      (should = my-var 1))))