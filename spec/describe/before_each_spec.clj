(ns specjure)

(describe before-each "without examples" 
  (before-each (set-params :var1 1)))

(describe before-each "with examples"
  (before-each (set-params :var1 1))

  (it "runs before-each function before each example"
    (should = 1 (params :var1)))

  (describe "with nesting"
    (it "runs nested examples"
      (should = 1 (params :var1)))
    
    (describe "with deeper nesting"
      (before-each 
	(set-params :var1 (inc (params :var1))))

      (it "runs deeply nested examples"
	(should = 2 (params :var1)))))

  (describe "with separate nesting"
    (it "runs separately nested examples"
      (should = 1 (params :var1)))))