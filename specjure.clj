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
(def *parameters*)
(defstruct example-group :desc :before-all-fns :before-each-fns 
	   :example-descs :example-fns :after-each-fns :after-all-fns)
(def *example-group* (struct example-group "" [] [] [] [] [] []))

;;; Verification
(defn parse-matcher [matcher & arguments]
  (cond (= matcher '=) arguments
	(= matcher 'be-true) [true (not (not (first arguments)))]
	(= matcher 'be-false) [false (not (not (first arguments)))]))

(defn format-failure [expected actual]
  (format "expected: %s%ngot: %s (using =)%n" expected actual))

(defn- check-example [group-desc example-desc example-fn]
  (try (example-fn)
       (print ".") true
       (catch java.lang.AssertionError e
	 (print "F")
	 (format "%n'%s%s' FAILED%n%s" group-desc example-desc (.getMessage e)))))

(defn- check-group [group]
  (binding [*parameters* {}]        
    (doseq fn (:before-all-fns group) (fn))
    (let [result
	  (doall (map (fn [example-desc example-fn] 
			(doseq fn (:before-each-fns group) (fn))
			(let [checked-example
			      (check-example (:desc group) example-desc example-fn)]
			  (doseq fn (:after-each-fns group) (fn))		    
			  checked-example))
		      (:example-descs group)
		      (:example-fns group)))]
      (doseq fn (:after-all-fns group) (fn))
      result)))

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
    `(binding [*example-group* (assoc *example-group* 
				 :desc (str (:desc *example-group*) ~group-desc " ")
				 :example-descs [] :example-fns [])]
       ~@body
       (dosync (commute *example-groups* conj *example-group*)))))

(defmacro _push-group! [key val]
  `(set! *example-group* (assoc *example-group* ~key (conj (~key *example-group*) ~val))))

(defmacro before-all [& body]
  `(_push-group! :before-all-fns (fn [] ~@body)))

(defmacro before-each [& body]
  `(_push-group! :before-each-fns (fn [] ~@body)))

(defmacro it [desc & body]
  `(do (_push-group! :example-descs ~desc)
       (_push-group! :example-fns (fn [] ~@body))))

(defmacro after-each [& body]
  `(_push-group! :after-each-fns (fn [] ~@body)))

(defmacro after-all [& body]
  `(_push-group! :after-all-fns (fn [] ~@body)))

(defmacro params [param]
  `(~param *parameters*))

(defmacro set-params [name value]
  `(set! *parameters* (assoc *parameters* ~name ~value)))

(defn- _should [comparator matcher arguments]
  `(let [[expected# actual#] (parse-matcher '~matcher ~@arguments)]
     (when-not (~comparator expected# actual#)
       (throw (new java.lang.AssertionError (format-failure expected# actual#))))))

(defmacro should [matcher & arguments]
  (_should '= matcher arguments))

(defmacro should-not [matcher & arguments]
  (_should '(complement =) matcher arguments))

(defn check
  ([] (check @*example-groups*))
  ([body]                    
     (let [examples (mapcat check-group body)
	   failures (filter string? examples)]    
       (dosync (ref-set *example-groups* []))
       (printf "%n%n%s Examples, %s Failures%n" (count examples) (count failures))
       (doseq failure failures (print failure)))))

(defn specdoc
  ([] (specdoc @*example-groups*))
  ([body]
     (dosync (ref-set *example-groups* []))
     (doseq example-group body
       (printf "%n%s%n" (:desc example-group))
       (doseq example (:example-descs example-group)
	 (printf "- %s%n" example)))))