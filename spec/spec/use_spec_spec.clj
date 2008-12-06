(ns specjure)

(share-spec "shared behavior without parameters" []
  (spec "runs shared examples without parameters"
    (ie = 1 1)))

(share-spec "shared behavior with parameters" [var1]
  (spec "runs shared examples with parameters"
    (ie = 1 var1)))

(spec use-spec
  (use-spec "shared behavior without parameters")
  (use-spec "shared behavior with parameters" 1)

  (spec "runs standard examples" (ie = 1 1)))