(ns specjure)

(describe should "with a be-predicate matcher" ()
  (describe "that evaluates to true" ()
    (it "passes example"
      (should be-pos 3)))

  (describe "that evaluates to false" ()
    (it "passes example"
      (should-not be-pos -3))))