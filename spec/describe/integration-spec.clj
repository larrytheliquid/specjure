(ns specjure)

(describe describe "with mulitple options combined" 
  (:before [my-var (inc my-var)]
   :before [my-var 1])
  
  (it "applies every option to examples"
    (should = my-var 2)))