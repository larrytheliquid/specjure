(ns specjure)

(describe after-all "without examples" 
  (after-all (set-param :var1 1)))

(describe after-all "with examples"
  (after-all (set-param :var1 1))

  (it "runs after-all function after all examples"
    (should = nil (param :var1)))

  (describe "with nesting"
    (it "runs nested examples"
      (should = nil (param :var1)))
    
    (describe "with deeper nesting"
      (after-all (set-param :var1 2))

      (it "runs deeply nested examples"
	(should = nil (param :var1)))))

  (describe "with separate nesting"
    (it "runs separately nested examples"
      (should = nil (param :var1)))))