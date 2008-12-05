(ns specjure)

(spec should "throw with a non-error"
  (it "fails example"
    (should not throw Error (rest []))))

(spec should "throw with an error value"
  (it "fails example"
    (should not throw Error Exception)))

(spec should "throw with a thrown error"
  (it "passes example for more general errors"
    (should throw Exception (/ 1 0)))

  (it "passes example for matching errors"
    (should throw java.lang.ArithmeticException (/ 1 0)))

  (it "fails example for non-matching errors"
    (should not throw java.lang.AssertionError (/ 1 0))))

