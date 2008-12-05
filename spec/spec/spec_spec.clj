(ns specjure)

(spec spec "without examples")

(spec spec "with examples"
  (it "runs single-line examples"
    (should = [1 2 3 4] 
	      (concat [1 2] [3 4])))

  (it "runs multi-line examples"
    (inc 2)
    (should = 3 (inc 2)))

  (it "runs multi-expectation examples"
    (should = 1 1)
    (should = 2 2))

  (spec "with nesting"
    (it "runs nested examples"
      (should = 1 1))
    
    (spec "with deeper nesting"
      (it "runs deeply nested examples"
	(should = 1 1))))

  (spec "with separate nesting"
    (it "runs separately nested examples"
      (should = 1 1))))