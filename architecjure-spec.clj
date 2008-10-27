(load-file "architecjure.clj")

(defn it-function []
  (it "should run requirement when called inside an architect"
    (=> (* 4 5 6) should = 120)))

(verify-requirements

(architect "architect, without requirements")

(architect "architect, with requirements"
  (it "should run a single-line requirement"
    (=> (concat [1 2] [3 4]) should = [1 2 3 4]))

  (it "should run a multi-line requirement"
    (inc 2)
    (=> (inc 2) should = 3))

  (it "should run a multi-expectation requirement"
    (=> 1 should = 1)
    (=> 2 should = 2)))

(architect "architect, with requirements in functions"
  (it-function))

(architect "architect, with root level"
  (it "should run requirements in the root level"
    (=> 1 should = 1))
  
  (architect "with nesting"
    (it "should run requirements in a nested level"
      (=> 1 should = 1))

    (architect "with deeper nesting"
      (it "should run requirements in the deepest level"
	(=> 1 should = 1))))

  (architect "with separate nesting"
    (it "should run separately nested requirements"
      (=> 1 should = 1))))

(architect "architect-requirement, with description"
  (it "should return the requirement prepended by the description"
    (let [requirement (struct requirement "description")
	  architected-requirement (architect-requirement {:description "modified"} requirement)]
      (=> (:description architected-requirement) should = "modified description"))))

)