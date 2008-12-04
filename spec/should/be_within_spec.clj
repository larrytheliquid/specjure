(ns specjure)

(describe should "be-close with lesser error"
  (it "on integers passes example"
    (should be-close 2 1 (/ 5 3)))

  (it " on floats passes example"
    (should be-close 1.6 0.1 (/ 5 3))))

(describe should "be-close with greater error"
  (it "on integers fails example"
    (should not be-close 3 1 (/ 5 3)))

  (it "on floats fails example"
    (should not be-close 1.5 0.1 (/ 5 3))))