(ns specjure)

(group should "throw with a non-error"
  (spec "fails example"
    (should not throw Error (rest []))))

(group should "throw with an error value"
  (spec "fails example"
    (should not throw Error Exception)))

(group should "throw with a thrown error"
  (spec "passes example for more general errors"
    (should throw Exception (/ 1 0)))

  (spec "passes example for matching errors"
    (should throw java.lang.ArithmeticException (/ 1 0)))

  (spec "fails example for non-matching errors"
    (should not throw java.lang.AssertionError (/ 1 0))))

