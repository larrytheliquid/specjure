(ns specjure)
(def *example-groups* (agent []))
(def *examples*)
(def *failed-expectations*)

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
	body (if (not function-str) args body)
	;; options
	all-options (concat options [:register-example-group true])
	each-options `(~'*group-description* ~description)]
    (reduce (fn [code [name value]]
	      (option {:option-name name
		       :option-value value
		       :code code}))
	    `(let [~@each-options] ~@body)
	    (partition 2 all-options))))

(defmacro it [description & body]
  `(push! *examples*
	  (struct example (str ~'*group-description* " " ~description)
		  #(binding [*failed-expectations* []]
		     ~@body
		     *failed-expectations*))))

(defmacro should [matcher & arguments]
  `(let [expectation# (apply struct expectation = (parse-matcher '~matcher ~@arguments))]
     (when-not (passed? expectation#)
       (push! *failed-expectations* expectation#))))

(defmacro should-not [matcher & arguments]
  `(let [expectation# (apply struct expectation (complement =) (parse-matcher '~matcher ~@arguments))]
     (when-not (passed? expectation#)
       (push! *failed-expectations* expectation#))))

(defn check-examples 
  ([] (await *example-groups*) (check-examples @*example-groups*))
  ([body]
     (send *example-groups* (fn [_] []))
     (let [examples (map check (mapcat #(apply %) body))
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