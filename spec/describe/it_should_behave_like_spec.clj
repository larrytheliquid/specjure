(ns specjure)

(shared-examples-for "shared behavior"
  (it "runs shared examples"
    (should = 1 1)))

(describe it-should-behave-like
  (it-should-behave-like "non-existent behavior")
  (it-should-behave-like "shared behavior")

  (it "runs standard examples"
    (should = 1 1)))