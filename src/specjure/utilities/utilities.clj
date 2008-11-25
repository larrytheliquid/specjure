(ns specjure)

(defmacro push! [coll x]
  (list 'set! coll (list 'conj coll x)))

(defn fn-ns-str [fn-sym]
  (let [ns-prefix (str (ns-name *ns*) "/")
	fn-str (str fn-sym)]
    (if (. fn-str (startsWith ns-prefix))
      fn-str
      (str ns-prefix fn-str))))