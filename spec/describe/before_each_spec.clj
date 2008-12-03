(ns specjure)

(group before-each "without examples" 
  (before-each ($assoc! :var1 1)))

(group before-each "with examples"
  (before-each ($assoc! :var1 1))

  (spec "runs before-each function before each example"
    (should = 1 ($get :var1)))

  (group "with nesting"
    (spec "runs nested examples"
      (should = 1 ($get :var1)))
    
    (group "with deeper nesting"
      (before-each 
	($assoc! :var1 (inc ($get :var1))))

      (spec "runs deeply nested examples"
	(should = 2 ($get :var1)))))

  (group "with separate nesting"
    (spec "runs separately nested examples"
      (should = 1 ($get :var1)))))