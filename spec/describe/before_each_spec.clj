(ns specjure)

(describe describe "with before-each and without examples" 
  (before-each (let [var1 1 var2 2])))

(describe describe "with before-each and examples"
  (before-each (let [var1 1 var2 2]))

  (it "binds all variables in examples"
    (should = 1 1)
    (should = 2 2)))