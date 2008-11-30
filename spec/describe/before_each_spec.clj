(ns specjure)

(describe describe "with before-each and without examples" 
  (before-each))

(describe describe "with before-each and examples"
  (before-each (set-params :var1 1))

  (it "runs before-each function before each example"
    (should = (params :var1) 1)))