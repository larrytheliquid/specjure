(ns specjure)

(describe should "be-within with lesser error"
  (it "on integers passes example"
    (should be-within 2 1 (/ 5 3)))

  (it " on floats passes example"
    (should be-within 1.6 0.1 (/ 5 3))))

(describe should "be-within with greater error"
  (it "on integers fails example"
    (should not be-within 3 1 (/ 5 3)))

  (it "on floats fails example"
    (should not be-within 1.5 0.1 (/ 5 3))))