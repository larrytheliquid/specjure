(ns specjure)

(group before-all "without examples" 
  (before-all ($assoc! :var1 1)))

(group before-all "with examples"
  (before-all ($assoc! :var1 1))

  (spec "runs before-all function before all examples"
    (should = 1 ($get :var1)))

  (group "with nesting"
    (spec "runs nested examples"
      (should = 1 ($get :var1)))
    
    (group "with deeper nesting"
      (before-all
	($assoc! :var1 (inc ($get :var1))))

      (spec "runs deeply nested examples"
	(should = 2 ($get :var1)))))

  (group "with separate nesting"
    (spec "runs separately nested examples"
      (should = 1 ($get :var1)))))