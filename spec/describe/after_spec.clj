(ns specjure)

(describe after "without examples" 
  (after ($assoc! :var1 1)))

(describe after "with examples"
  (after ($assoc! :var1 1))

  (it "runs after function after each example"
    (should = nil ($get :var1)))

  (describe "with nesting"
    (it "runs nested examples"
      (should = nil ($get :var1)))
    
    (describe "with deeper nesting"
      (after ($assoc! :var1 2))

      (it "runs deeply nested examples"
	(should = nil ($get :var1)))))

  (describe "with separate nesting"
    (it "runs separately nested examples"
      (should = nil ($get :var1)))))