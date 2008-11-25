(ns specjure)

(describe describe "with multiple options combined" 
  (:before-each [var1 1]
   :before [var2 2])
  
  (it "applies every option to examples"
    (should = var1 1)
    (should = var2 2)))