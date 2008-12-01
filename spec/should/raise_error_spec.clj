(ns specjure)

(describe should "raise-error with a non-error"
  (it "fails example"
    (should not raise-error Error (rest []))))

(describe should "raise-error with an error value"
  (it "fails example"
    (should not raise-error Error Exception)))

(describe should "raise-error with a raised error"
  (it "passes example for more general errors"
    (should raise-error Exception (/ 1 0)))

  (it "passes example for matching errors"
    (should raise-error java.lang.ArithmeticException (/ 1 0)))

  (it "fails example for non-matching errors"
    (should not raise-error java.lang.AssertionError (/ 1 0))))

