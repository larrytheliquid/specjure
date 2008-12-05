(ns specjure)

(spec ie "= with equal arguments"
  (it "passes example"
    (ie = 3 3)))

(spec ie "= without equal arguments"
  (it "fails example"
    (ie not = 3 7)))