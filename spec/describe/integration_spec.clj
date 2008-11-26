(ns specjure)

(describe describe "with multiple options combined" 
  (:before [var1 1])
  
  (it "applies every option to examples"
    (should = var1 1)))