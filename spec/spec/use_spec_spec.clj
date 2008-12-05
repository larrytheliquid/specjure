(ns specjure)

(share-spec "shared behavior without parameters" []
  (it "runs shared examples without parameters"
    (ie = 1 1)))

(share-spec "shared behavior with parameters" [var1]
  (it "runs shared examples with parameters"
    (ie = 1 var1)))

(spec use-spec
  (use-spec "non-existent behavior")
  (use-spec "shared behavior without parameters")
  (use-spec "shared behavior with parameters" 1)

  (it "runs standard examples"
    (ie = 1 1)))