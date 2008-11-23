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

(describe describe "with the before option and a list of let bindings and body, and with examples"
  (:before ([var1 1 ref1 (ref 0)] 
	      (dosync (commute ref1 inc))))

  (it "binds all variables and runs body in examples"
    (should = var1 1)
    (should = @ref1 1))
  
  (describe "that are nested with another vector of let bindings and body" 
    (:before ([var1 (- var1) ref2 (ref 1)]
		(dosync (commute ref2 inc))))

    (it "re-binds all variables in nested examples"
      (should = var1 -1)
      (should = @ref1 1)
      (should = @ref2 2))

    (describe "deeply" ()
      (it "re-binds all variables in deeply nested examples"
	(should = var1 -1)
	(should = @ref1 1)
	(should = @ref2 2)))))