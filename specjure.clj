(ns specjure)

;;; Utilities
(defn fn-ns-str [fn-sym]
  (let [ns-prefix (str (ns-name *ns*) "/")
	fn-str (str fn-sym)]
    (if (. fn-str (startsWith ns-prefix))
      fn-str
      (str ns-prefix fn-str))))

;;; Data
(def *example-groups* (ref []))
(def *description*)
(def *examples*)
(defstruct example-group :type :description :examples)
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
(defmethod check ::ExampleGroup [example-group]
  (map (fn [example] (check example)) (:examples example-group)))
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
	       *examples* []] 
       ~@body
       (dosync (commute *example-groups* conj (struct example-group ::ExampleGroup 
						      *description* *examples*))))))

(defmacro it [description & body]
  `(set! *examples*  (conj *examples* 
			   (struct example ::Example 
				   (str *description* " " ~description)
				   (fn [] ~@body)))))

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