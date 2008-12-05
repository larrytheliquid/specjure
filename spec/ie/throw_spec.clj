(ns specjure)

(spec ie "throw with a non-error"
  (it "fails example"
    (ie not throw Error (rest []))))

(spec ie "throw with an error value"
  (it "fails example"
    (ie not throw Error Exception)))

(spec ie "throw with a thrown error"
  (it "passes example for more general errors"
    (ie throw Exception (/ 1 0)))

  (it "passes example for matching errors"
    (ie throw java.lang.ArithmeticException (/ 1 0)))

  (it "fails example for non-matching errors"
    (ie not throw java.lang.AssertionError (/ 1 0))))

