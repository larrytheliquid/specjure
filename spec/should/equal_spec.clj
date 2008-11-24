(ns specjure)

(describe should "= with equal arguments" ()
  (it "passes example"
    (should = 3 3)))

(describe should "= without equal arguments" ()
  (it "fails example"
    (should-not = 3 7)))