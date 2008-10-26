;;; Utilities
(defmacro push! [coll x]
  (list 'set! coll (list 'conj coll x)))

;;; Data structures
(def *example-groups*)
(def *failed-expectations*)

(defstruct example-group :type :description :examples)
(defmacro create-example-group [description examples]
  `(struct example-group :example-group ~description ~examples))

(defstruct example :type :description :behavior)
(defmacro create-example [description behavior]
  `(struct example :example ~description ~behavior))

(defstruct expectation :type :expected :actual :result)
(defmacro create-expectation [expected actual]
  `(struct expectation :expectation ~expected ~actual))

;;; Verification
(defmulti verify :type)
(defmethod verify :example-group [example-group]
  (map verify (map (fn [example] 
		     (assoc example :description 
			    (str (:description example-group) " " (:description example))))
		   (:examples example-group))))

(defmethod verify :example [example]
  (let [failed-expectations ((:behavior example)) example-passed? (empty? failed-expectations)]
    (printf "%s%s%n" (:description example) (if example-passed? "" " (FAILED)"))
    (if example-passed? example (assoc example :failed-expectations failed-expectations))))

(defmethod verify :expectation [expectation]
  (= (:expected expectation) (:actual expectation)))

;;; Public interface
(defmacro describe [description & body]
  `(push! *example-groups* (create-example-group ~description (list ~@body))))

(defmacro it [description & behavior]
  `(create-example ~description (fn [] (binding [*failed-expectations* []]
					 ~@behavior
					 *failed-expectations*))))

(defmacro => [expected should matcher actual]
  `(let [expectation# (create-expectation ~expected ~actual)]
     (when-not (verify expectation#)
       (push! *failed-expectations* expectation#))))

(defmacro run-examples [& body]
  `(binding [*example-groups* []]
     (do ~@body)
     (let [examples# (mapcat verify *example-groups*)
	   examples-count# (count examples#)
	   failures-count# (count (filter :failed-expectations examples#))]
       (printf "%n%s Examples, %s Failures%n" examples-count# failures-count#))))