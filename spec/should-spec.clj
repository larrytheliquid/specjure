(ns specjure)

(describe "specjure/should with the = matcher"
  (describe "with equal arguments"
    (it "returns true"
      (should = 3 3))))

(describe "specjure/should with the be-false matcher"
  (describe "with false"
    (it "returns true"
      (should be-false false))))

(describe "specjure/should with the be-true matcher"
  (describe "with true"
    (it "returns true"
      (should be-true true))))

(describe "specjure/should with a be-predicate matcher"
  (describe "with pos? and a positive number"
    (it "returns true"
      (should be-pos 3))))