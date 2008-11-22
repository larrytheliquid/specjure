(ns specjure)

(describe specjure/should-not "with the = matcher" ()
  (describe "without equal arguments" ()
    (it "passes example"
      (should-not = 3 7))))

(describe specjure/should-not "with a be-predicate matcher" ()
  (describe "that evaluates to false" ()
    (it "passes example"
      (should-not be-pos -3))))