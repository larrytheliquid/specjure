(ns specjure)

;;; Utilities
(defn fn-ns-str [fn-sym]
  (let [ns-prefix (str (ns-name *ns*) "/")
	fn-str (str fn-sym)]
    (if (. fn-str (startsWith ns-prefix))
      fn-str
      (str ns-prefix fn-str))))

(defmacro push! [coll x]
  (list 'set! coll (list 'conj coll x)))

;;; Data
(def *example-groups* (ref []))
(def *parameters*)
(def *group-desc*)
(def *before-all-fns*)
(def *before-each-fns*)
(def *example-fns*)
(def *after-each-fns*)
(def *after-all-fns*)
(defstruct example-group :type :desc :before-all-fns :before-each-fns 
	   :example-fns :after-each-fns :after-all-fns)
(defstruct example :type :desc :fn)
(defstruct expectation :comparator :actual :expected)

;;; Verification
(defn parse-matcher [matcher & arguments]
  (cond (= matcher '=) arguments
	(= matcher 'be-true) [true (not (not (first arguments)))]
	(= matcher 'be-false) [false (not (not (first arguments)))]))

(defn format-failure [expectation]
  (format "expected: %s%ngot: %s (using =)%n" 
	  (:actual expectation) (:expected expectation)))

(defmulti check :type)

(defmethod check ::ExampleGroup [group]
  (binding [*parameters* {}]    
    (doseq i (:before-all-fns group) (i))
    (let [result
	  (doall (map (fn [example-fn] 
			(doseq i (:before-each-fns group) (i))
			(check example-fn)
			(doseq i (:after-each-fns group) (i))) 
		      (:example-fns group)))]
      (doseq i (:after-all-fns group) (i))
      result)))

(defmethod check ::Example [example]
  (try ((:fn example))
       (print ".") true
       (catch java.lang.AssertionError e
	 (print "F")
	 (format "%n'%s' FAILED%n%s" 
		 (:desc example)
		 (.getMessage e)))))

;;; Interface
(defmacro describe 
  "Describes a specification in the form of verifiable (executable) examples."
  {:arglists '([symbol? description? (options*) examples*])} 
  [arg1 arg2 & args]  
  (let [function-str (when (symbol? arg1) (fn-ns-str arg1))	
	group-desc (if (string? arg2) (str function-str " " arg2) function-str)
	body (if (string? arg2) args (cons arg2 args))
	group-desc (if (not function-str) arg1 group-desc)
	body (if (not function-str) (cons arg2 args) body)]
    `(binding [*group-desc* ~group-desc
	       *before-all-fns* []
	       *before-each-fns* []
	       *example-fns* []
	       *after-each-fns* []
	       *after-all-fns* []] 
       ~@body
       (dosync (commute *example-groups* conj (struct example-group ::ExampleGroup 
						      *group-desc* 
						      *before-all-fns*
						      *before-each-fns*
						      *example-fns*
						      *after-each-fns*
						      *after-all-fns*))))))

(defmacro before-all [& body]
  `(push! *before-all-fns* (fn [] ~@body)))

(defmacro before-each [& body]
  `(push! *before-each-fns* (fn [] ~@body)))

(defmacro it [desc & body]
  `(push! *example-fns* (struct example ::Example 
				(str *group-desc* " " ~desc)
				(fn [] ~@body))))

(defmacro after-each [& body]
  `(push! *after-each-fns* (fn [] ~@body)))

(defmacro after-all [& body]
  `(push! *after-all-fns* (fn [] ~@body)))

(defmacro params [param]
  `(~param *parameters*))

(defmacro set-params [name value]
  `(set! *parameters* (assoc *parameters* ~name ~value)))

(defn- _should [comparator matcher arguments]
  `(let [expectation# (apply struct expectation ~comparator (parse-matcher '~matcher ~@arguments))]
     (when-not ((:comparator expectation#) (:actual expectation#) (:expected expectation#))
       (throw (new java.lang.AssertionError (format-failure expectation#))))))

(defmacro should [matcher & arguments]
  (_should '= matcher arguments))

(defmacro should-not [matcher & arguments]
  (_should '(complement =) matcher arguments))

(defn check-examples 
  ([] (check-examples @*example-groups*))
  ([body]                    
     (let [examples (mapcat check body)
	   failures (filter string? examples)]    
       (dosync (ref-set *example-groups* []))
       (printf "%n%n%s Examples, %s Failures%n" (count examples) (count failures))
       (doseq failure failures (print failure)))))