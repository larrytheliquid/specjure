(ns specjure)

(shared-group "shared behavior without parameters" []
  (spec "runs shared examples without parameters"
    (should = 1 1)))

(shared-group "shared behavior with parameters" [var1]
  (spec "runs shared examples with parameters"
    (should = 1 var1)))

(group it-should-behave-like
  (spec-behaves-like "non-existent behavior")
  (spec-behaves-like "shared behavior without parameters")
  (spec-behaves-like "shared behavior with parameters" 1)

  (spec "runs standard examples"
    (should = 1 1)))