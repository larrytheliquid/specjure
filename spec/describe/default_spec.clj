(ns specjure)

(describe describe "by default without examples" ())

(describe describe "by default with examples" ()
  (it "runs single-line examples"
    (should = (concat [1 2] [3 4]) [1 2 3 4]))

  (it "runs multi-line examples"
    (inc 2)
    (should = (inc 2) 3))

  (it "runs multi-expectation examples"
    (should = 1 1)
    (should = 2 2))
  
  (describe "that are nested" ()
    (it "runs nested examples"
      (should = 1 1))

    (describe "deeply" ()
      (it "runs deeply nested examples"
	(should = true true))))

  (describe "that are nested separately" ()
    (it "runs separately nested examples"
      (should = false false))))