(ns specjure)

(describe before-all "without examples" 
  (before-all (set-param :var1 1)))

(describe before-all "with examples"
  (before-all (set-param :var1 1))

  (it "runs before-all function before all examples"
    (should = 1 (param :var1)))

  (describe "with nesting"
    (it "runs nested examples"
      (should = 1 (param :var1)))
    
    (describe "with deeper nesting"
      (before-all
	(set-param :var1 (inc (param :var1))))

      (it "runs deeply nested examples"
	(should = 2 (param :var1)))))

  (describe "with separate nesting"
    (it "runs separately nested examples"
      (should = 1 (param :var1)))))