(ns specjure)

(spec should "be-predicate with clojure/pos?"
  (spec "with a positive number"
    (it "passes example when resolved"
      (should be pos 3))

    (it "passes example when fully qualified"
      (should be clojure/pos 3)))

  (spec "with a negnative number"
    (it "fails example when resolved"
      (should not be pos -3))

    (it "fails example when fully qualified"
      (should not be clojure/pos -3))))