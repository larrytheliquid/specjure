(ns specjure)

(describe before-all "without examples" 
  (before-all (set-params :var1 1)))

(describe before-all "with examples"
  (before-all (set-params :var1 1))

  (it "runs before-all function before all examples"
    (should = 1 (params :var1))))