(ns specjure)

(describe should "be-true with true" ()
  (it "passes example"
    (should be-true true)))

(describe should "be-true with an empty list" ()
  (it "passes example"
    (should be-true ())))

(describe should "be-true with other objects" ()
  (it "passes example"
    (should be-true 3)
    (should be-true "string")
    (should be-true {:one 1})))

(describe should "be-true with false" ()
  (it "fails example"
    (should-not be-true false)))

(describe should "be-true with nil" ()
  (it "fails example"
    (should-not be-true nil)))