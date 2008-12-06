(ns specjure)

(spec before "without examples" (before ($assoc! :var1 1)))

(spec before "with examples" (before ($assoc! :var1 1))
  (spec "runs before function before each example"
    (ie = 1 ($get :var1)))

  (spec "with nesting runs nested examples"
    (ie = 1 ($get :var1))
    
    (spec "with deeper nesting runs deeply nested examples"
      (before ($assoc! :var1 (inc ($get :var1))))

      (ie = 2 ($get :var1))))

  (spec "with separate nesting runs separately nested examples"
    (ie = 1 ($get :var1))))