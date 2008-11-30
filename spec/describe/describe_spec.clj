(ns specjure)

(describe describe "without examples")

(describe describe "with examples"
  (it "runs single-line examples"
    (should = (concat [1 2] [3 4]) [1 2 3 4]))

  (it "runs multi-line examples"
    (inc 2)
    (should = (inc 2) 3))

  (it "runs multi-expectation examples"
    (should = 1 1)
    (should = 2 2)))