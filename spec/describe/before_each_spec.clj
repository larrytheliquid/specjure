(ns specjure)

(describe before-each "with before-each and without examples" 
  (before-each))

(describe before-each "with before-each and examples"
  (before-each (set-params :var1 1))

  (it "runs before-each function before each example"
    (should = (params :var1) 1)))