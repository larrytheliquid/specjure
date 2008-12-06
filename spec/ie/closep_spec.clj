(ns specjure)

;; (spec ie "be-close with lesser error"
;;   (it "on integers passes example"
;;     (ie be-close 2 1 (/ 5 3)))

;;   (it " on floats passes example"
;;     (ie be-close 1.6 0.1 (/ 5 3))))

;; (spec ie "be-close with greater error"
;;   (it "on integers fails example"
;;     (ie not be-close 3 1 (/ 5 3)))

;;   (it "on floats fails example"
;;     (ie not be-close 1.5 0.1 (/ 5 3))))