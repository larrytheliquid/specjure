(ns specjure)

;;; Utilities
(defn fn-ns-str [fn-sym]
  (let [ns-prefix (str (ns-name *ns*) "/")
	fn-str (str fn-sym)]
    (if (.contains fn-str "/")
      fn-str
      (str ns-prefix fn-str))))

(defn close? [expected delta actual]
  (and (>= (+ expected delta) actual) 
       (< (- expected delta) actual)))

(defmacro throws? [exception & body]
  `(try (do ~@body) false
       (catch ~exception e# true)
       (catch Error e# false)
       (catch Exception e# false)))

;;; Data
(def *example-groups* (atom []))
(def *shared-groups* (atom {}))
(def *parameters*)
(defstruct example-group :desc :befores :examples :afters)
(def *example-group* (struct example-group "" [] [] []))

;;; Verification
(defn format-failure [expected actual]
  (format "expected: %s%ngot: %s" expected actual))

(defn- check-example [desc example]
  (try (let [result (example)]
	 (if result
	   (do (print "F") (format "%n%s[FAILURE]%n%s%n" desc result))
	   (do (print ".") true)))
       (catch java.lang.Exception e
	 (print "E")
	 (format "%n%s[ERROR]%n%s: %s%n" desc 
		 (.getName (.getClass e)) (.getMessage e)))))

(defn- check-group [group]
  (binding [*parameters* {}]        
    (doall (map (fn [example] 
		  (doseq [f (:befores group)] (f))
		  (let [checked-example
			(check-example (:desc group) example)]
		    (doseq [f (:afters group)] (f))		    
		    checked-example))
		(:examples group)))))

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
    `(binding [*example-group* (assoc *example-group* :examples []
				 :desc (str (:desc *example-group*) ~group-desc " "))]
       ~@body
       (swap! *example-groups* conj *example-group*))))

(defmacro _push-group! [key val]
  `(set! *example-group* (assoc *example-group* ~key (conj (~key *example-group*) ~val))))

(defmacro before [& body]
  `(_push-group! :befores (fn [] ~@body)))

(defmacro after [& body]
  `(_push-group! :afters (fn [] ~@body)))

(defmacro share-spec [desc params & body]
  `(swap! *shared-groups* assoc ~desc (fn [~@params] ~@body)))

(defmacro use-spec [desc & args]
  `(let [f# (get @*shared-groups* ~desc)]
     (when f# (f# ~@args))))

(defmacro $get [param]
  `(~param *parameters*))

(defmacro $assoc! [name value]
  `(set! *parameters* (assoc *parameters* ~name ~value)))

(defmacro _ie [bol f & args]
  `(when (= ~bol (not (~f ~@args)))
     (format-failure true false)))

(defmacro ie [& body]
  `(_push-group! :examples (fn [] (_ie true ~@body))))

(defmacro ie-not [& body]
  `(_push-group! :examples (fn [] (_ie false ~@body))))

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
       (doseq [failure failures] (print failure)))))

(defn specdoc
  ([] (specdoc @*example-groups*))
  ([body] (doseq [example-group body]
	    (printf "%n- %s" (:desc example-group)))))

(defn check-file [path & options]
  (swap! *example-groups* (fn [_] []))
  (swap! *shared-groups* (fn [_] {}))
  (let [options (if (empty? options) {} (apply assoc {} options))]
    (let [file (java.io.File. path)]
      (if (and (not (.isHidden file)) (.isDirectory file))
	((fn loader [file]
	   (cond (and (not (.isHidden file)) (.isDirectory file)) 
		   (doseq [file (.listFiles file)] (loader file))
		 (and (not (.isHidden file)) (.endsWith (.getName file) "_spec.clj")) 
		   (load-file (.getPath file)))
	   ) file)
	(load-file (.getPath file))))
    (cond (:dry-run options) (specdoc)
	  true (check))))
