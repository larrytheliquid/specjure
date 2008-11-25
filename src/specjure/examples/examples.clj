(ns specjure)
(defstruct example :tag :description :fn)

(defmulti check :tag)

(defmethod check ::ExampleGroup [example-group]
  ((:fn example-group)))

(defmethod check ::Example [example]
  (let [failed-expectations ((:fn example))
	example-passed? (empty? failed-expectations)]
    (printf "%s%s%n" (:description example) (if example-passed? "" " (FAILED)"))
    (if example-passed? example 
	(assoc example :failed-expectations failed-expectations))))