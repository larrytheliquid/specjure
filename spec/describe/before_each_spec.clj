(ns specjure)

(describe before-each "without examples" 
  (before-each ($assoc! :var1 1)))

(describe before-each "with examples"
  (before-each ($assoc! :var1 1))

  (it "runs before-each function before each example"
    (should = 1 ($get :var1)))

  (describe "with nesting"
    (it "runs nested examples"
      (should = 1 ($get :var1)))
    
    (describe "with deeper nesting"
      (before-each 
	($assoc! :var1 (inc ($get :var1))))

      (it "runs deeply nested examples"
	(should = 2 ($get :var1)))))

  (describe "with separate nesting"
    (it "runs separately nested examples"
      (should = 1 ($get :var1)))))