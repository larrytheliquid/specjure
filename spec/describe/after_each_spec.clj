(ns specjure)

(group after-each "without examples" 
  (after-each ($assoc! :var1 1)))

(group after-each "with examples"
  (after-each ($assoc! :var1 1))

  (spec "runs after-each function after each example"
    (should = nil ($get :var1)))

  (group "with nesting"
    (spec "runs nested examples"
      (should = nil ($get :var1)))
    
    (group "with deeper nesting"
      (after-each ($assoc! :var1 2))

      (spec "runs deeply nested examples"
	(should = nil ($get :var1)))))

  (group "with separate nesting"
    (spec "runs separately nested examples"
      (should = nil ($get :var1)))))