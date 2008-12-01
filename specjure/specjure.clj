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
(def *shared-examples* (ref {}))
(def *parameters*)
(defstruct example-group :desc :before-all-fns :before-each-fns 
	   :example-descs :example-fns :after-each-fns :after-all-fns)
(def *example-group* (struct example-group "" [] [] [] [] [] []))

(defmacro be-predicate [pred & args]
  `((resolve (symbol (str '~pred \?))) ~@args))

;;; Verification
(defmacro parse-matcher [matcher & args]
  (cond (= matcher '=) `(vector ~@args)
	(= matcher 'be) `(vector true (be-predicate ~@args))
	(= matcher 'throw) `(vector true
	   (try ~@args false
		(catch ~(first args) e#
		  true)
		(catch Error e#
		  false)
		(catch Exception e#
		  false)))
	true (throw Exception "Unsupported matcher")))

(defn format-failure [expected actual]
  (format "expected: %s%ngot: %s (using =)" expected actual))

(defn- check-example [group-desc example-desc example-fn]
  (try (when example-fn 
	 (example-fn)
	 (print ".") true)
       (when-not example-fn
	 (print "P")
	 (format "%n'%s%s' PENDING%n" group-desc example-desc))
       (catch java.lang.Exception e
	 (print "E")
	 (format "%n'%s%s' ERROR%n%s: %s%n" group-desc example-desc 
		 (.getName (.getClass e)) (.getMessage e)))
       (catch java.lang.AssertionError e
	 (print "F")
	 (format "%n'%s%s' FAILURE%n%s%n" group-desc example-desc (.getMessage e)))))

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
       (_push-group! :example-fns (when-not (empty? '~body) (fn [] ~@body)))))

(defmacro after-each [& body]
  `(_push-group! :after-each-fns (fn [] ~@body)))

(defmacro after-all [& body]
  `(_push-group! :after-all-fns (fn [] ~@body)))

(defmacro shared-examples-for [desc params & body]
  `(dosync (commute *shared-examples* assoc ~desc (fn [~@params] ~@body))))

(defmacro it-should-behave-like [desc & args]
  `(let [fn# (get @*shared-examples* ~desc)]
     (when fn# (fn# ~@args))))

(defmacro param [param]
  `(~param *parameters*))

(defmacro set-param [name value]
  `(set! *parameters* (assoc *parameters* ~name ~value)))

(defn- _should [comparator matcher arguments]
  `(let [[expected# actual#] (parse-matcher ~matcher ~@arguments)]
     (when-not (~comparator expected# actual#)
       (throw (new java.lang.AssertionError (format-failure expected# actual#))))))

(defmacro should [matcher & arguments]
  (if (= matcher 'not)
    (_should '(complement =) (first arguments) (rest arguments))
    (_should '= matcher arguments)))

(defn check
  ([] (check @*example-groups*))
  ([body]                    
     (let [examples (mapcat check-group body)
	   num-examples (count examples)
	   failures (filter string? examples)
	   num-failures (count failures)]       
       (printf "%n%n%s Example%s, %s Failure%s%n" 
	       num-examples (if (= 1 num-examples) "" "s")
	       num-failures (if (= 1 num-failures) "" "s"))
       (doseq failure failures (print failure)))))

(defn specdoc
  ([] (specdoc @*example-groups*))
  ([body]
     (doseq example-group body
       (printf "%n%s%n" (:desc example-group))
       (doseq example (:example-descs example-group)
	 (printf "- %s%n" example)))))

(defn spec [path & options]
  (dosync (ref-set *example-groups* []))
  (dosync (ref-set *shared-examples* {}))
  (let [options (if (empty? options) {} (apply assoc {} options))]
    (let [file (java.io.File. path)]
      (if (and (not (.isHidden file)) (.isDirectory file))
	((fn loader [file]
	   (cond (and (not (.isHidden file)) (.isDirectory file)) 
		   (doseq file (.listFiles file) (loader file))
		 (and (not (.isHidden file)) (.endsWith (.getName file) "_spec.clj")) 
		   (load-file (.getPath file)))
	   ) file)
	(load-file (.getPath file))))
    (cond (:dry-run options) (specdoc)
	  true (check))))
