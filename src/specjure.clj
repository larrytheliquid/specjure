(ns specjure)

;;; Utilities
(defn close? [expected delta actual]
  (and (>= (+ expected delta) actual) 
       (< (- expected delta) actual)))

(defmacro throws? [exception & body]
  `(try (do ~@body) false
       (catch ~exception e# true)
       (catch Error e# false)
       (catch Exception e# false)))

(defn fn-ns-str [fn-sym]
  (let [ns-prefix (str (ns-name *ns*) "/")
	fn-str (str fn-sym)]
    (if (.contains fn-str "/")
      fn-str
      (str ns-prefix fn-str))))

(defn- join
  ([values] (join values " "))
  ([values separator] (join values separator ""))
  ([values separator start]
     (if (= 0 (count values))
       ""
       (str start (reduce 
		   (fn [acc e] (str acc separator (print-str e))) 
		   values)))))


;;; Data
(def *example-groups* (atom []))
(def *shared-groups* (atom {}))
(def *parameters*)
(defstruct example-group :desc :befores :examples :afters)
(def *example-group* (struct example-group "" [] [] []))

;;; Interface
(defmacro spec 
  "Create a specification in the form of verifiable (executable) examples."
  {:arglists '([symbol? description? body])} 
  [arg1 arg2 & args]  
  (let [function-str (when (symbol? arg1) (fn-ns-str arg1))	
	group-desc (if (string? arg2) (str function-str " " arg2) function-str)
	body (if (string? arg2) args (cons arg2 args))
	group-desc (if (not function-str) arg1 group-desc)
	body (if (not function-str) (cons arg2 args) body)]
    `(binding [*example-group* (assoc *example-group* 
				 :examples []
				 :desc (str (:desc *example-group*) 
					    ~group-desc " "))]
       ~@body
       (swap! *example-groups* conj *example-group*))))

(defmacro share-spec [desc params & body]
  `(swap! *shared-groups* assoc ~desc (fn [~@params] ~@body)))

(defmacro use-spec [desc & args]
  `(let [f# (get @*shared-groups* ~desc)]
     (when f# (f# ~@args))))

(defmacro _push-group! [key val]
  `(set! *example-group* (assoc *example-group* ~key 
				(conj (~key *example-group*) ~val))))

(defmacro before [& body]
  `(_push-group! :befores (fn [] ~@body)))

(defmacro after [& body]
  `(_push-group! :afters (fn [] ~@body)))

(defmacro $get [param]
  `(~param *parameters*))

(defmacro $assoc! [name value]
  `(set! *parameters* (assoc *parameters* ~name ~value)))

(defmulti expectation 
  (fn [bol f args] f))

(defmethod expectation :default [bol f args]
  `(let [value# (~f ~@args)]
     (when (= ~bol (not value#))
       (format-failure '~f ~bol "<logical true>" value#))))

(defmethod expectation '= [bol f args]
  `(let [args# (list ~@args)]
     (when (= ~bol (not (apply ~f args#)))
       (format-failure ~bol '~f (first args#) 
		       (join (rest args#) "")))))

(defn format-failure [bol f expected actual]
  (format "using: %s%nexpected%s: %s%ngot: %s" 
	  f (if bol "" "-not") 
	  expected actual))

(defmacro _ie [bol f & args]
  (expectation bol f args))

(defmacro ie [& body]
  `(_push-group! :examples (fn [] (_ie true ~@body))))

(defmacro ie-not [& body]
  `(_push-group! :examples (fn [] (_ie false ~@body))))

;;; Verification
(defn- check-example [desc example]
  (try (if-let [result (example)]
	 (do (print "F") (format "%n%s[FAILURE]%n%s%n" desc result))
	 (do (print ".") true))
       (catch java.lang.Exception e
	 (print "E")
	 (format "%n%s[ERROR]%n%s: %s%n" desc 
		 (.getName (.getClass e)) (.getMessage e)))))

(defn- check-group [group]
  (map #(binding [*parameters* {}]
	  (doseq [f (:befores group)] (f))
	  (let [result (check-example (:desc group) %)]
	    (doseq [f (:afters group)] (f))		    
	    result))
       (:examples group)))

(defn- check
  ([] (check @*example-groups*))
  ([body]                    
     (let [examples (mapcat check-group body)
	   num-examples (count examples)
	   failures (filter string? examples)
	   num-failures (count failures)]       
       (printf "%n%n%s Example%s, %s Failure%s%n" 
	       num-examples (if (= 1 num-examples) "" "s")
	       num-failures (if (= 1 num-failures) "" "s"))
       (doseq [failure failures] (print failure)))))

(defn- specdoc
  ([] (specdoc @*example-groups*))
  ([body] (doseq [example-group body]
	    (printf "%n- %s" (:desc example-group)))))

(defn check-file [path & options]
  (swap! *example-groups* (fn [_] []))
  (swap! *shared-groups* (fn [_] {}))
  (let [options (if (empty? options) {} (apply assoc {} options))]
    (let [file (.getAbsoluteFile (java.io.File. path))]
      (if (and (not (.isHidden file)) (.isDirectory file))
	((fn loader [file]
	   (cond (and (not (.isHidden file)) (.isDirectory file)) 
		 (doseq [file (.listFiles file)] (loader file))
		 (and (not (.isHidden file)) 
		      (.endsWith (.getName file) "_spec.clj")) 
		 (load-file (.getPath file)))) file)
	(load-file (.getPath file))))
    (cond (:dry-run options) (specdoc)
	  true (check))))
