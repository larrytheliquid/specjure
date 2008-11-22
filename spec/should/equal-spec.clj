(ns specjure)

(describe should "with the = matcher" ()
  (describe "with equal arguments" ()
    (it "passes example"
      (should = 3 3)))

  (describe "without equal arguments" ()
    (it "fails example"
      (should-not = 3 7))))