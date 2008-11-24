(ns specjure)
(defmulti option :option-name)

(defmethod option :register-example-group [{code :code}]
  `(send *example-groups* conj (fn [] ~code)))


(defmethod option :before [{val :option-value code :code}]
  (let [bindings (if (list? val) (first val) val)
	body (when (list? val) (rest val))]
    `(let [~@bindings]
       ~@body
       ~code)))