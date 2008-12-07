(ns specjure)

(spec close? "with lesser error"
  (spec "with an integer passes example"
    (ie close? 2 1 (/ 5 3)))

  (spec "with a float passes example"
    (ie close? 1.6 0.1 (/ 5 3))))

(spec close? "with greater error"
  (spec "with an integer fails example"
    (ie-not close? 3 1 (/ 5 3)))

  (spec "with a float fails example"
    (ie-not close? 1.5 0.1 (/ 5 3))))