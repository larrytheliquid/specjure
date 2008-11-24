(ns specjure)
(defmulti option :option-name)

(defmethod option :register-example-group [{code :code}]
  `(send *example-groups* conj #(binding [*examples* []]
				  ~code
				  *examples*)))

(defmethod option :before [{val :option-value code :code}]
  (let [bindings (if (list? val) (first val) val)
	body (when (list? val) (rest val))]
    `(let [~@bindings]
       ~@body
       ~code)))