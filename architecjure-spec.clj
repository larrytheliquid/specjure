(load-file "architecjure.clj")

(run-examples

(def my-vector)
(describe "push!, with an empty vector"
  (it "should add the value to the list"
    (binding [my-vector []]
      (push! my-vector 1)
      (=> my-vector should = [1]))))

(describe "push!, with a non-empty vector"
  (it "should add the value to the back of the list"
    (binding [my-vector [1]]
      (push! my-vector 2)
      (=> my-vector should = [1 2]))))

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

(defn it-function []
  (it "should run example when called inside a describe"
    (=> (* 4 5 6) should = 120)))

(describe "describe, with examples in functions"
  (it-function))

)