;;; Data structures
(defstruct unit :type :example-groups)
(defmacro create-unit [example-groups]
  `(struct unit :unit ~example-groups))

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
(defmethod verify :unit [unit]
  (doseq example-group (:example-groups unit) (verify example-group)))

(defmethod verify :example-group [example-group]
  (printf "%n%s%n"(:description example-group))
  (doseq example (:examples example-group) (verify example)))

(defmethod verify :example [example]
  (printf "- %s%s%n" (:description example)
	             (if (verify (:behavior example)) "" (format " (FAILED)"))))

(defmethod verify :expectation [expectation]
  (= (:expected expectation) (:actual expectation)))

;;; Public interface
(defmacro describe [description & body]
  `(create-example-group ~description (list ~@body)))

(defmacro it [description & behavior]
  `(create-example ~description (do ~@behavior)))

(defmacro => [expected should matcher actual]
  `(create-expectation ~expected ~actual))

(defn run-examples [& example-groups]
  (verify (create-unit example-groups)))