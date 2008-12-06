(ns specjure)

(spec spec "without examples")

(spec spec "with examples runs examples"
  (ie = [1 2 3 4] (concat [1 2] [3 4]))

  (spec "with nesting runs nested examples"
    (ie = 1 1)
    
    (spec "with deeper nesting runs deeply nested examples"
      (ie = 1 1)))

  (spec "with separate nesting runs separately nested examples"
    (ie = 1 1)))