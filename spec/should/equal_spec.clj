(ns specjure)

(spec should "= with equal arguments"
  (it "passes example"
    (should = 3 3)))

(spec should "= without equal arguments"
  (it "fails example"
    (should not = 3 7)))