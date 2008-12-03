(ns specjure)

(describe after-all "without examples" 
  (after-all ($assoc! :var1 1)))

(describe after-all "with examples"
  (after-all ($assoc! :var1 1))

  (it "runs after-all function after all examples"
    (should = nil ($get :var1)))

  (describe "with nesting"
    (it "runs nested examples"
      (should = nil ($get :var1)))
    
    (describe "with deeper nesting"
      (after-all ($assoc! :var1 2))

      (it "runs deeply nested examples"
	(should = nil ($get :var1)))))

  (describe "with separate nesting"
    (it "runs separately nested examples"
      (should = nil ($get :var1)))))