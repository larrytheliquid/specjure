(ns specjure)

(describe should "be-false with false"
  (it "passes example"
    (should be-false false)))

(describe should "be-false with nil"
  (it "passes example"
    (should be-false nil)))

(describe should "be-false with an empty list"
  (it "fails example"
    (should-not be-false ())))

(describe should "be-false with other objects"
  (it "fails example"
    (should-not be-false 3)
    (should-not be-false "string")
    (should-not be-false {:one 1})))

(describe should "be-false with true"
  (it "fails example"
    (should-not be-false true)))