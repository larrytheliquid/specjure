(ns specjure)

(spec before "without examples" 
  (before ($assoc! :var1 1)))

(spec before "with examples"
  (before ($assoc! :var1 1))

  (it "runs before function before each example"
    (ie = 1 ($get :var1)))

  (spec "with nesting"
    (it "runs nested examples"
      (ie = 1 ($get :var1)))
    
    (spec "with deeper nesting"
      (before 
	($assoc! :var1 (inc ($get :var1))))

      (it "runs deeply nested examples"
	(ie = 2 ($get :var1)))))

  (spec "with separate nesting"
    (it "runs separately nested examples"
      (ie = 1 ($get :var1)))))