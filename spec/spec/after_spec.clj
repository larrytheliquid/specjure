(ns specjure)

(spec after "without examples" 
  (after ($assoc! :var1 1)))

(spec after "with examples"
  (after ($assoc! :var1 1))

  (it "runs after function after each example"
    (ie = nil ($get :var1)))

  (spec "with nesting"
    (it "runs nested examples"
      (ie = nil ($get :var1)))
    
    (spec "with deeper nesting"
      (after ($assoc! :var1 2))

      (it "runs deeply nested examples"
	(ie = nil ($get :var1)))))

  (spec "with separate nesting"
    (it "runs separately nested examples"
      (ie = nil ($get :var1)))))