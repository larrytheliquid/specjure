;;; Data structures
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

(defmulti verify :type)
(defmethod verify :example-group [example-group]
  (printf "%n%s%n"(:description example-group))
  (map (fn [example] 
	 (assoc example :description 
		(str (:description example-group) " " (:desription example))))
       (map verify (:examples example-group))))

(defmethod verify :example [example]
  (let [failed-expectations ((:behavior example)) example-passed? (empty? failed-expectations)]
    (printf "- %s%s%n" (:description example) (if example-passed? "" " (FAILED)"))
    (if example-passed? example (assoc example :failed-expectations failed-expectations))))

(defmethod verify :expectation [expectation]
  (= (:expected expectation) (:actual expectation)))

;;; Public interface
(defmacro describe [description & body]
  `(create-example-group ~description (list ~@body)))

(defmacro it [description & behavior]
  `(create-example ~description (fn [] (binding [*failed-expectations* []]
					 ~@behavior
					 *failed-expectations*))))

(defmacro => [expected should matcher actual]
  `(when-not (= ~expected ~actual) 
     (set! *failed-expectations*
	   (conj *failed-expectations* (create-expectation ~expected ~actual)))))

(defn run-examples [& example-groups]
  (let [examples (mapcat verify example-groups)
	examples-count (count examples)
	failures-count (count (filter :failed-expectations examples))]
    (printf "%n%s Examples, %s Failures%n" examples-count failures-count)))