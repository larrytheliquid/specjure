(load-file "architecjure.clj")

(run-examples

(describe "describe, without examples")

(describe "describe, with 1 example"
  (it "should run a single 1 line example"
    (verify (concat [1 2] [3 4]) should = [1 2 3 4])))
)