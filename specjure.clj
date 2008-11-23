(ns specjure)

;;; Utilities
(defn flatten [x]
  (let [s? #(instance? clojure.lang.Sequential %)]
    (filter (complement s?)
	    (tree-seq s? seq x))))

(defmacro push! [coll x]
  (list 'set! coll (list 'conj coll x)))

;;; Data structures
(def *examples* (agent []))
(def *describe-nests* [])
(def *failed-expectations*)
(defstruct example :description :behavior)
(defstruct expectation :comparator :actual :expected)

;;; Verification
(defn check [example]
  (let [failed-expectations ((:behavior example))
	example-passed? (empty? failed-expectations)]
    (printf "%s%s%n" (:description example) (if example-passed? "" " (FAILED)"))
    (if example-passed? example 
	(assoc example :failed-expectations failed-expectations))))

(defn passed? [expectation]
  ((:comparator expectation) (:expected expectation) (:actual expectation)))

;;; describe options
(defmulti option :option-name)
(defmethod option :before [{val :option-value code :code}]
  (let [bindings (if (list? val) (first val) val)
	body (when (list? val) (rest val))]
    `(let [~@bindings]
       ~@body
       ~code)))

(defn fn-ns-str [fn-sym]
  (let [ns-prefix (str (ns-name *ns*) "/")
	fn-str (str fn-sym)]
    (if (. fn-str (startsWith ns-prefix))
      fn-str
      (str ns-prefix fn-str))))

;;; Public interface
(defmacro describe 
  "Describes a specification in the form of verifiable (executable) examples."
  {:arglists '([fn-sym? description? (options*) body])} 
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
    (reduce (fn [code [name value]]
	      (option {:option-name name
		       :option-value value
		       :code code}))
	    `(binding [*describe-nests* (concat *describe-nests* [~description " "])]
	       (flatten (list ~@body)))
	    (partition 2 options))))

(defmacro it [description & body]
  `(let [example#
	 (struct example (str (reduce str *describe-nests*) ~description)
		         #(binding [*failed-expectations* []]
			    ~@body
			    *failed-expectations*))]
     (send *examples* conj example#)
     example#))

(defmacro should [matcher & arguments]
  `(let [expectation# (apply struct expectation = (parse-matcher '~matcher ~@arguments))]
     (when-not (passed? expectation#)
       (push! *failed-expectations* expectation#))))

(defmacro should-not [matcher & arguments]
  `(let [expectation# (apply struct expectation (complement =) (parse-matcher '~matcher ~@arguments))]
     (when-not (passed? expectation#)
       (push! *failed-expectations* expectation#))))

(defn be-function [be-matcher]
  (resolve (symbol (apply str 
			  (conj (vec (drop 3 (str be-matcher))) \?)))))

(defn parse-matcher [matcher & arguments]
  (cond (= matcher '=) arguments
	(= (symbol (apply str (take 3 (str matcher))))  'be-) 
	[(apply (be-function matcher)  arguments) true]))

(defn check-examples 
  ([] (await *examples*) (check-examples @*examples*))
  ([body]
     (send *examples* (fn [_] []))
     (let [examples (map check body)
	   examples-count (count examples)
	   failures-count (count (filter :failed-expectations examples))]
       (printf "%n%s Examples, %s Failures%n" examples-count failures-count)
       (doseq failed-example (filter :failed-expectations examples)
	 (doseq failed-expectation (:failed-expectations failed-example)
	   (printf "%n'%s' FAILED%nexpected: %s%ngot: %s (using =)%n" 
		   (:description failed-example)
		   (:expected failed-expectation)
		   (:actual failed-expectation)))))))

(defn spec [path]
  "Run examples in the specified file, or files ending in -spec
  in a directory and its recursive subdirectories."
  (load-file path) (check-examples))