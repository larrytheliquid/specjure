(ns specjure)

(group should "be-predicate with clojure/pos?"
  (group "with a positive number"
    (spec "passes example when resolved"
      (should be pos 3))

    (spec "passes example when fully qualified"
      (should be clojure/pos 3)))

  (group "with a negnative number"
    (spec "fails example when resolved"
      (should not be pos -3))

    (spec "fails example when fully qualified"
      (should not be clojure/pos -3))))