(ns specjure)

(describe specjure/should "with the = matcher" ()
  (describe "with equal arguments" ()
    (it "passes example"
      (should = 3 3))))

(describe specjure/should "with a be-predicate matcher" ()
  (describe "that evaluates to true" ()
    (it "passes example"
      (should be-pos 3))))