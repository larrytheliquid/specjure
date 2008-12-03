(ns specjure)

(group should "= with equal arguments"
  (spec "passes example"
    (should = 3 3)))

(group should "= without equal arguments"
  (spec "fails example"
    (should not = 3 7)))