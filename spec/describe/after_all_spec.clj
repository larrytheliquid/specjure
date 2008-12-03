(ns specjure)

(group after-all "without examples" 
  (after-all ($assoc! :var1 1)))

(group after-all "with examples"
  (after-all ($assoc! :var1 1))

  (spec "runs after-all function after all examples"
    (should = nil ($get :var1)))

  (group "with nesting"
    (spec "runs nested examples"
      (should = nil ($get :var1)))
    
    (group "with deeper nesting"
      (after-all ($assoc! :var1 2))

      (spec "runs deeply nested examples"
	(should = nil ($get :var1)))))

  (group "with separate nesting"
    (spec "runs separately nested examples"
      (should = nil ($get :var1)))))