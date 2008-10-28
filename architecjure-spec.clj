(load-file "architecjure.clj")

(defn it-function []
  (it "should run example when called inside an describe"
    (=> (* 4 5 6) should = 120)))

(check-examples

(describe "describe, without examples")

(describe "describe, with examples"
  (it "should run a single-line example"
    (=> (concat [1 2] [3 4]) should = [1 2 3 4]))

  (it "should run a multi-line example"
    (inc 2)
    (=> (inc 2) should = 3))

  (it "should run a multi-expectation example"
    (=> 1 should = 1)
    (=> 2 should = 2)))

(describe "describe, with examples in functions"
  (it-function))

(describe {:description "describe, with root level"
	   :wrap-each (fn [example] `(let [~'my-var 1] ~example))}
  (it "should run examples in the root level"
    (=> my-var should = 1))
  
  (describe "with nesting"
    (it "should run examples in a nested level"
      (=> my-var should = 1))

    (describe {:description "with deeper nesting"
	       :wrap-each (fn [example] `(let [~'my-var (inc ~'my-var)] ~example))}
      (it "should run examples in the deepest level"
	(=> my-var should = 2))))

  (describe "with separate nesting"
    (it "should run separately nested examples"
      (=> my-var should = 1))))

(describe "describe-example, with description"
  (it "should return the example prepended by the description"
    (let [example (struct example "description")
	  describeed-example (describe-example {:description "modified"} example)]
      (=> (:description describeed-example) should = "modified description"))))

)