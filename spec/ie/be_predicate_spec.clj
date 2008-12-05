(ns specjure)

(spec ie "be-predicate with clojure/pos?"
  (spec "with a positive number"
    (it "passes example when resolved"
      (ie be pos 3))

    (it "passes example when fully qualified"
      (ie be clojure/pos 3)))

  (spec "with a negnative number"
    (it "fails example when resolved"
      (ie not be pos -3))

    (it "fails example when fully qualified"
      (ie not be clojure/pos -3))))