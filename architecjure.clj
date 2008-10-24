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

;;; Public interface
(defmacro describe [description & body]
  `(create-example-group ~description (list ~@body)))

(defmacro it [description & behavior]
  `(create-example ~description "mock behavior"))

(defn run-examples [& example-groups]
  (doseq example-group example-groups
    (println example-group)))

;;; Output

(defmulti verify :type)
(defmethod verify :unit [unit]
  (doseq example-group (:example-groups unit) (verify example-group)))
(defmethod verify :example-group [example-group]
  (println (:description example-group))
  (doseq example (:examples example-group) (verify example)))
(defmethod verify :example [example]
  (println (:description example)))