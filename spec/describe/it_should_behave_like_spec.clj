(ns specjure)

(shared-examples-for "shared behavior without parameters" []
  (it "runs shared examples without parameters"
    (should = 1 1)))

(shared-examples-for "shared behavior with parameters" [var1]
  (it "runs shared examples with parameters"
    (should = 1 var1)))

(describe it-should-behave-like
  (it-should-behave-like "non-existent behavior")
  (it-should-behave-like "shared behavior without parameters")
  (it-should-behave-like "shared behavior with parameters" 1)

  (it "runs standard examples"
    (should = 1 1)))