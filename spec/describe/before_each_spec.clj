(ns specjure)

;; (describe describe "with :before-each, a vector of let bindings, and without examples" 
;;   (before-each (let [var1 1 var2 2])))

;; (describe describe "with :before-each, a vector of let bindings, and examples"
;;   (before-each (let [var1 1 var2 2]))

;;   (it "binds all variables in examples"
;;     (should = var1 1)
;;     (should = var2 2)))

;; (describe describe "with :before-each, a list of let bindings and body, and without examples"
;;   (before-each (let [var1 1 ref1 (ref 0)] 
;; 		 (dosync (commute ref1 inc)))))

;; (describe describe "with :before-each, a list of let bindings and body, and examples"
;;   (before-each (let [var1 1 ref1 (ref 0)] 
;; 		 (dosync (commute ref1 inc))))

;;   (it "binds all variables and runs body in examples"
;;     (should = var1 1)
;;     (should = @ref1 1)))