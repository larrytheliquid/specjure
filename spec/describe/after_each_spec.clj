(ns specjure)

(describe after-each "without examples" 
  (after-each (set-param :var1 1)))

(describe after-each "with examples"
  (after-each (set-param :var1 1))

  (it "runs after-each function after each example"
    (should = nil (param :var1)))

  (describe "with nesting"
    (it "runs nested examples"
      (should = nil (param :var1)))
    
    (describe "with deeper nesting"
      (after-each (set-param :var1 2))

      (it "runs deeply nested examples"
	(should = nil (param :var1)))))

  (describe "with separate nesting"
    (it "runs separately nested examples"
      (should = nil (param :var1)))))