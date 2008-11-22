(ns specjure)

(describe describe "with the before option and without examples" ())

(describe describe "with the before option and a vector of let bindings, and with examples"
  (:before [var1 1 var2 2])

  (it "binds all variables in examples"
    (should = var1 1)
    (should = var2 2))
  
  (describe "that are nested with another vector of let bindings" 
    (:before [var2 (- var2) var3 3])

    (it "re-binds all variables in nested examples"
      (should = var1 1)
      (should = var2 -2)
      (should = var3 3))

    (describe "deeply" ()
      (it "re-binds all variables in deeply nested examples"
	(should = var1 1)
	(should = var2 -2)
	(should = var3 3)))))