(ns specjure)

(describe after-all "without examples" 
  (after-all (set-params :var1 1)))

(describe after-all "with examples"
  (after-all (set-params :var1 1))

  (it "runs after-all function after all examples"
    (should = nil (params :var1))))