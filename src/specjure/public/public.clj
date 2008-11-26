(ns specjure)
(def *examples* (ref []))
(def *description*)
(def *wrappers*)
(def *failed-expectations*)

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
		    (struct example ::Example (str *description* " " ~description)
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