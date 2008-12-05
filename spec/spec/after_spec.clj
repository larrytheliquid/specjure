(ns specjure)

(spec after "without examples" 
  (after ($assoc! :var1 1)))

(spec after "with examples"
  (after ($assoc! :var1 1))

  (it "runs after function after each example"
    (should = nil ($get :var1)))

  (spec "with nesting"
    (it "runs nested examples"
      (should = nil ($get :var1)))
    
    (spec "with deeper nesting"
      (after ($assoc! :var1 2))

      (it "runs deeply nested examples"
	(should = nil ($get :var1)))))

  (spec "with separate nesting"
    (it "runs separately nested examples"
      (should = nil ($get :var1)))))