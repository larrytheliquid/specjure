(ns specjure)

(describe before-all "without examples" 
  (before-all ($assoc! :var1 1)))

(describe before-all "with examples"
  (before-all ($assoc! :var1 1))

  (it "runs before-all function before all examples"
    (should = 1 ($get :var1)))

  (describe "with nesting"
    (it "runs nested examples"
      (should = 1 ($get :var1)))
    
    (describe "with deeper nesting"
      (before-all
	($assoc! :var1 (inc ($get :var1))))

      (it "runs deeply nested examples"
	(should = 2 ($get :var1)))))

  (describe "with separate nesting"
    (it "runs separately nested examples"
      (should = 1 ($get :var1)))))