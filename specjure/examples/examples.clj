(ns specjure)
(defstruct example :description :behavior)

(defn check [example]
  (let [failed-expectations ((:behavior example))
	example-passed? (empty? failed-expectations)]
    (printf "%s%s%n" (:description example) (if example-passed? "" " (FAILED)"))
    (if example-passed? example 
	(assoc example :failed-expectations failed-expectations))))