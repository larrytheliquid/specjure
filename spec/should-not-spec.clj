(ns specjure)

(describe "specjure/should-not with the = matcher"
  (describe "with unequal arguments"
    (it "returns true"
      (should-not = 3 5))))

(describe "specjure/should-not with the be-false matcher"
  (describe "with true"
    (it "returns true"
      (should-not be-false true))))

(describe "specjure/should-not with the be-true matcher"
  (describe "with false"
    (it "returns true"
      (should-not be-true false))))

(describe "specjure/should-not with a be-predicate matcher"
  (describe "with pos? and a negative number"
    (it "returns true"
      (should-not be-pos -3))))