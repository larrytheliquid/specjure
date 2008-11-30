(ns specjure)

(describe before-each "without examples" 
  (before-each (set-params :var1 1)))

(describe before-each "with examples"
  (before-each (set-params :var1 1))

  (it "runs before-each function before each example"
    (should = 1 (params :var1))))