(ns specjure)
(defmulti option :option-name)

(defmethod option :register-example-group [{val :option-value code :code}]
  `(send *example-groups* conj
	 (struct example ::ExampleGroup
		 ~val
		 (fn [] ~code))))

(defmethod option :parse-example [{code :code}]
  (vec (map (fn [example]	 	 
	      `(let []
		 ~example)) 
	    code)))

(defmethod option :add-description [{val :option-value code :code}]
  (vec (map (fn [example]
	      `(let [example# ~example]
		 (assoc example# :description
			(str ~val " " (:description example#))))) 
	    code)))

(defmethod option :before [{val :option-value code :code}]
  (let [bindings (if (list? val) (first val) val)
	body (when (list? val) (rest val))]
    `(let [~@bindings]
       ~@body
       ~code)))

(defmethod option :before-each [{val :option-value code :code}]
  (let [bindings (if (list? val) (first val) val)
	body (when (list? val) (rest val))]
    (vec (map (fn [example]
		`(let [~@bindings]
		   ~@body
		   ~example)) 
	      code))))