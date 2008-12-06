(ns specjure)

(spec throws? "with a non-error fails example"
  (ie-not throws? Error (rest [])))

(spec throws? "throw with an error value fails example"
  (ie-not throws? Error Exception))

(spec throws? "throw with a thrown error"
  (spec "passes example for more general errors"
    (ie throws? Exception (/ 1 0)))

  (spec "passes example for matching errors"
    (ie throws? java.lang.ArithmeticException (/ 1 0)))

  (spec "fails example for non-matching errors"
    (ie-not throws? java.lang.AssertionError (/ 1 0))))