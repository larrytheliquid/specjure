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
(def *group-desc*) (def *parameters*)
(def *before-all-fns*) (def *before-each-fns*)
(def *example-descs*) (def *example-fns*)
(def *after-each-fns*) (def *after-all-fns*)
(defstruct example-group :desc :before-all-fns :before-each-fns 
	   :example-descs :example-fns :after-each-fns :after-all-fns)

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

(defn check [group]
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
    `(binding [*group-desc* ~group-desc *before-all-fns* [] *before-each-fns* []
	       *example-descs* [] *example-fns* [] *after-each-fns* [] *after-all-fns* []]
       ~@body
       (let [group#
	     (struct example-group *group-desc* *before-all-fns* *before-each-fns*
		     *example-descs* *example-fns* *after-each-fns* *after-all-fns*)]
	 (dosync (commute *example-groups* conj group#))))))

(defmacro before-all [& body]
  `(push! *before-all-fns* (fn [] ~@body)))

(defmacro before-each [& body]
  `(push! *before-each-fns* (fn [] ~@body)))

(defmacro it [desc & body]
  `(do (push! *example-descs* ~desc)
       (push! *example-fns* (fn [] ~@body))))

(defmacro after-each [& body]
  `(push! *after-each-fns* (fn [] ~@body)))

(defmacro after-all [& body]
  `(push! *after-all-fns* (fn [] ~@body)))

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

(defn check-examples 
  ([] (check-examples @*example-groups*))
  ([body]                    
     (let [examples (mapcat check body)
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