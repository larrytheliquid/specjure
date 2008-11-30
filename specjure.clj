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
(def *description*)
(def *parameters*)
(def *before-alls*)
(def *before-eachs*)
(def *examples*)
(def *after-eachs*)
(def *after-alls*)
(defstruct example-group :type :description :before-alls :before-eachs 
	   :examples :after-eachs :after-alls)
(defstruct example :type :description :fn)
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
    (doseq before-all (:before-alls group) (before-all))
    (let [result
	  (doall (map (fn [example] 
			(doseq before-each (:before-eachs group) (before-each))
			(check example)
			(doseq after-each (:after-eachs group) (after-each))) 
		      (:examples group)))]
      (doseq after-all (:after-alls group) (after-all))
      result)))

(defmethod check ::Example [example]
  (try ((:fn example))
       (print ".") true
       (catch java.lang.AssertionError e
	 (print "F")
	 (format "%n'%s' FAILED%n%s" 
		 (:description example)
		 (.getMessage e)))))

;;; Interface
(defmacro describe 
  "Describes a specification in the form of verifiable (executable) examples."
  {:arglists '([symbol? description? (options*) examples*])} 
  [arg1 arg2 & args]  
  (let [function-str (when (symbol? arg1) (fn-ns-str arg1))	
	description (if (string? arg2) (str function-str " " arg2) function-str)
	body (if (string? arg2) args (cons arg2 args))
	description (if (not function-str) arg1 description)
	body (if (not function-str) (cons arg2 args) body)]
    `(binding [*description* ~description
	       *before-alls* []
	       *before-eachs* []
	       *examples* []
	       *after-eachs* []
	       *after-alls* []] 
       ~@body
       (dosync (commute *example-groups* conj (struct example-group ::ExampleGroup 
						      *description* 
						      *before-alls*
						      *before-eachs*
						      *examples*
						      *after-eachs*
						      *after-alls*))))))

(defmacro before-all [& body]
  `(push! *before-alls* (fn [] ~@body)))

(defmacro before-each [& body]
  `(push! *before-eachs* (fn [] ~@body)))

(defmacro it [description & body]
  `(push! *examples* (struct example ::Example 
			     (str *description* " " ~description)
			     (fn [] ~@body))))

(defmacro after-each [& body]
  `(push! *after-eachs* (fn [] ~@body)))

(defmacro after-all [& body]
  `(push! *after-alls* (fn [] ~@body)))

(defmacro params [param]
  `(~param *parameters*))

(defmacro set-params [name value]
  `(set! *parameters* (assoc *parameters* ~name ~value)))

(defn _should [comparator matcher arguments]
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