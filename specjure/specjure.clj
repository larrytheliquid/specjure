(ns specjure)

;;; Utilities
(defn fn-ns-str [fn-sym]
  (let [ns-prefix (str (ns-name *ns*) "/")
	fn-str (str fn-sym)]
    (if (.contains fn-str "/")
      fn-str
      (str ns-prefix fn-str))))

;;; Data
(def *example-groups* (ref []))
(def *shared-examples* (ref {}))
(def *parameters*)
(defstruct example-group :desc :before-fns 
	   :example-descs :example-fns :after-fns)
(def *example-group* (struct example-group "" [] [] [] []))

(defmacro be-predicate [pred & args]
  `(~(resolve (symbol (str pred \?))) ~@args))

(defmacro be-close [expected delta actual]
  `(and (> (+ ~expected ~delta) ~actual) (< (- ~expected ~delta) ~actual)))

;;; Verification
(defmacro parse-matcher [matcher & args]
  (cond (= matcher '=) `(vector ~@args)
	(= matcher 'be) `(vector true (be-predicate ~@args))
	(= matcher 'be-close) `(vector true (be-close ~@args))
	(= matcher 'throw) `(vector true
	   (try ~@args false
		(catch ~(first args) e#
		  true)
		(catch Error e#
		  false)
		(catch Exception e#
		  false)))
	true (throw (new Exception))))

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
    (doall (map (fn [example-desc example-fn] 
		  (doseq fn (:before-fns group) (fn))
		  (let [checked-example
			(check-example (:desc group) example-desc example-fn)]
		    (doseq fn (:after-fns group) (fn))		    
		    checked-example))
		(:example-descs group)
		(:example-fns group)))))

;;; Interface
(defmacro spec 
  "Create a specification in the form of verifiable (executable) examples."
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
       (dosync (alter *example-groups* conj *example-group*)))))

(defmacro _push-group! [key val]
  `(set! *example-group* (assoc *example-group* ~key (conj (~key *example-group*) ~val))))

(defmacro before [& body]
  `(_push-group! :before-fns (fn [] ~@body)))

(defmacro it [desc & body]
  `(do (_push-group! :example-descs ~desc)
       (_push-group! :example-fns (when-not (empty? '~body) (fn [] ~@body)))))

(defmacro after [& body]
  `(_push-group! :after-fns (fn [] ~@body)))

(defmacro share-spec [desc params & body]
  `(dosync (alter *shared-examples* assoc ~desc (fn [~@params] ~@body))))

(defmacro use-spec [desc & args]
  `(let [fn# (get @*shared-examples* ~desc)]
     (when fn# (fn# ~@args))))

(defmacro $get [param]
  `(~param *parameters*))

(defmacro $assoc! [name value]
  `(set! *parameters* (assoc *parameters* ~name ~value)))

(defn- _ie [comparator matcher arguments]
  `(let [[expected# actual#] (parse-matcher ~matcher ~@arguments)]
     (when-not (~comparator expected# actual#)
       (throw (new java.lang.AssertionError (format-failure expected# actual#))))))

(defmacro ie [matcher & arguments]
  (if (= matcher 'not)
    (_ie '(complement =) (first arguments) (rest arguments))
    (_ie '= matcher arguments)))

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

(defn verify [path & options]
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
