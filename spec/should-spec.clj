(ns specjure)

(desc should "with the = matcher" ()
  (desc "with equal arguments" ()
    (it "passes example"
      (should = 3 3))))

(desc should "with a be-predicate matcher" ()
  (desc "that evaluates to true" ()
    (it "passes example"
      (should be-pos 3))))