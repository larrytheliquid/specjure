(ns specjure)

;;; Utilities
(defmacro push! [coll x]
  (list 'set! coll (list 'conj coll x)))

(defn fn-ns-str [fn-sym]
  (let [ns-prefix (str (ns-name *ns*) "/")
	fn-str (str fn-sym)]
    (if (. fn-str (startsWith ns-prefix))
      fn-str
      (str ns-prefix fn-str))))

;;; Data
(def *examples* (ref []))
(def *description*)
(def *wrappers*)
(def *failed-expectations*)
(defstruct example :description :fn)
(defstruct expectation :comparator :actual :expected)

;;; Verification
(defn passed? [expectation]
  ((:comparator expectation) (:expected expectation) (:actual expectation)))

(defn parse-matcher [matcher & arguments]
  (cond (= matcher '=) arguments
	(= matcher 'be-true) [true (not (not (first arguments)))]
	(= matcher 'be-false) [false (not (not (first arguments)))]))

(defn check [example]
  (let [failed-expectations ((:fn example))
	example-passed? (empty? failed-expectations)]
    (print (if example-passed? "." "F"))
    (if example-passed? example 
	(assoc example :failed-expectations failed-expectations))))

;;; Options
(defmulti option :option-name)
(defmethod option :before [{val :option-value code :code}]
  (let [bindings (if (list? val) (first val) val)
	body (when (list? val) (rest val))]
    `(let [~@bindings]
       ~@body
       ~code)))

;;; Interface
(defmacro describe 
  "Describes a specification in the form of verifiable (executable) examples."
  {:arglists '([symbol? description? (options*) examples*])} 
  [arg1 arg2 & args]  
  (let [;; describing a function
	function-str (when (symbol? arg1) (fn-ns-str arg1))	
	description (if (string? arg2) (str function-str " " arg2) function-str)
	options (if (string? arg2) (first args) arg2)
	body (if (string? arg2) (rest args) args)
	;; describing anything else
	description (if (not function-str) arg1 description)
	options (if (not function-str) arg2 options)
	body (if (not function-str) args body)]
    `(binding [*description* ~description
	       *wrappers* (second ~options)] 
       ~@body)))

(defmacro it [description & body]
  `(dosync (commute *examples* conj
		    (struct example (str *description* " " ~description)
			    #(binding [*failed-expectations* []]
			       ~@body
			       *failed-expectations*)))))

(defn- _should [comparator matcher arguments]
  `(let [expectation# (apply struct expectation ~comparator (parse-matcher '~matcher ~@arguments))]
     (when-not (passed? expectation#)
       (push! *failed-expectations* expectation#))))

(defmacro should [matcher & arguments]
  (_should '= matcher arguments))

(defmacro should-not [matcher & arguments]
  (_should '(complement =) matcher arguments))

(defn check-examples 
  ([] (check-examples @*examples*))
  ([body]                    
     (dosync (ref-set *examples* []))
     (let [examples (map check body)
	   examples-count (count examples)
	   failures-count (count (filter :failed-expectations examples))]
       (printf "%n%n%s Examples, %s Failures%n" examples-count failures-count)
       (doseq failed-example (filter :failed-expectations examples)
	 (doseq failed-expectation (:failed-expectations failed-example)
	   (printf "%n'%s' FAILED%nexpected: %s%ngot: %s (using =)%n" 
		   (:description failed-example)
		   (:expected failed-expectation)
		   (:actual failed-expectation)))))))