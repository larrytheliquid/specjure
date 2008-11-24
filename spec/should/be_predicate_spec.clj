(ns specjure)

(describe should "be-predicate that evaluates to true" ()
  (it "passes example"
    (should be-pos 3)))

(describe should "be-predicate that evaluates to false" ()
  (it "passes example"
    (should-not be-pos -3)))