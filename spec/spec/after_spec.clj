(ns specjure)

(spec after "without examples" (after ($assoc! :var1 1)))

(spec after "with examples" (after ($assoc! :var1 1))
  (spec "runs after function after each example"
    (ie = nil ($get :var1)))

  (spec "with nesting runs nested examples"
    (ie = nil ($get :var1))
    
    (spec "with deeper nesting runs deeply nested examples"
      (after ($assoc! :var1 2))

      (ie = nil ($get :var1))))

  (spec "with separate nesting runs separately nested examples"
    (ie = nil ($get :var1))))