(load-file "architecjure.clj")

(run-examples

(describe "describe, without examples")

(describe "describe, with examples"
  (it "should run a single-line example"
    (=> (concat [1 2] [3 4]) should = [1 2 3 4]))

  (it "should run a multi-line example"
    (inc 2)
    (=> (inc 2) should = 3))

  (it "should run a multi-expectation example"
    (=> 1 should = 1)
    (=> 2 should = 2)))
)