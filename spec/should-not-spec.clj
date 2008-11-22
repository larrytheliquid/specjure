(ns specjure)

(desc should-not "with the = matcher" ()
  (desc "without equal arguments" ()
    (it "passes example"
      (should-not = 3 7))))

(desc should-not "with a be-predicate matcher" ()
  (desc "that evaluates to false" ()
    (it "passes example"
      (should-not be-pos -3))))