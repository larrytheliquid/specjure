(ns specjure)

(defn it-function []
  (it "runs examples when called inside a describe"
    (should = (* 4 5 6) 120)))

(describe describe "without examples" ())

(describe describe "with examples" ()
  (it "runs a single-line example"
    (should = (concat [1 2] [3 4]) [1 2 3 4]))

  (it "runs a multi-line example"
    (inc 2)
    (should = (inc 2) 3))

  (it "runs a multi-expectation example"
    (should = 1 1)
    (should = 2 2)))

(describe describe "with examples in functions" ()
  (it-function))