(ns specjure)

(describe after-each "without examples" 
  (after-each (set-params :var1 1)))

(describe after-each "with examples"
  (after-each (set-params :var1 1))

  (it "runs after-each function after each example"
    (should = nil (params :var1))))