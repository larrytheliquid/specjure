(ns specjure)

(group group "without examples")

(group group "with examples"
  (spec "runs single-line examples"
    (should = [1 2 3 4] 
	      (concat [1 2] [3 4])))

  (spec "runs multi-line examples"
    (inc 2)
    (should = 3 (inc 2)))

  (spec "runs multi-expectation examples"
    (should = 1 1)
    (should = 2 2))

  (group "with nesting"
    (spec "runs nested examples"
      (should = 1 1))
    
    (group "with deeper nesting"
      (spec "runs deeply nested examples"
	(should = 1 1))))

  (group "with separate nesting"
    (spec "runs separately nested examples"
      (should = 1 1))))