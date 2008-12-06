(ns specjure)

(spec ie "with a function returning logical true passes example"
  (ie = 3 3))

(spec ie "with a function returning logical false fails example"
  (ie-not = 3 7))