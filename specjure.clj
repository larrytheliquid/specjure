;;; Data structures
(def *failed-expectations*)
(defstruct example :description :behavior)
(defstruct expectation :comparator :expected :actual)

;;; Public interface
(defmacro describe [description options & body]
  (if (symbol? description) 
    (let [description (str description " " options)
	  options (first body)
	  body (rest body)]
      `(let [~@options]
	 (map (fn [example#] 
		(assoc example# :description (str ~description " " (:description example#)))) 
	      (flatten (list ~@body)))))
    `(let [~@options]
       (map (fn [example#] 
	      (assoc example# :description (str ~description " " (:description example#)))) 
	    (flatten (list ~@body))))))

(defn describe-macroexpansion []
  '(let [~@options]
     (map (fn [example#] 
	    (assoc example# :description (str ~description " " (:description example#)))) 
	  (flatten (list ~@body)))))

(defmacro it [description & behavior]
  `(struct example ~description #(binding [*failed-expectations* []]
					 ~@behavior
					 *failed-expectations*)))

(defmacro should [matcher & arguments]
  `(let [expectation# (apply struct expectation = (parse-matcher '~matcher ~@arguments))]
     (when-not (passed? expectation#)
       (push! *failed-expectations* expectation#))))

(defmacro should-not [matcher & arguments]
  `(let [expectation# (apply struct expectation (complement =) (parse-matcher '~matcher ~@arguments))]
     (when-not (passed? expectation#)
       (push! *failed-expectations* expectation#))))

(defn parse-matcher [matcher & arguments]
  (cond (= matcher '=) arguments
	(= (symbol (apply str (take 3 (str matcher))))  'be-) 
	  [(apply (eval (symbol (apply str 
				       (conj (vec (drop 3 (str matcher))) \?)))) arguments) true]))

(defmacro check-examples [& body]
  `(let [examples# (map check (concat ~@body))
	 examples-count# (count examples#)
	 failures-count# (count (filter :failed-expectations examples#))]
     (printf "%n%s Examples, %s Failures%n" examples-count# failures-count#)
     (doseq failed-example# (filter :failed-expectations examples#)
       (doseq failed-expectation# (:failed-expectations failed-example#)
	 (printf "%n'%s' FAILED%nexpected: %s%ngot: %s (using =)%n" 
	       (:description failed-example#)
	       (:expected failed-expectation#)
	       (:actual failed-expectation#))))))

;;; Verification
(defn check [example]
  (let [failed-expectations ((:behavior example))
	example-passed? (empty? failed-expectations)]
    (printf "%s%s%n" (:description example) (if example-passed? "" " (FAILED)"))
    (if example-passed? example 
	(assoc example :failed-expectations failed-expectations))))

(defn passed? [expectation]
  ((:comparator expectation) (:expected expectation) (:actual expectation)))

;;; Utilities
(defn flatten [x]
  (let [s? #(instance? clojure.lang.Sequential %)]
    (filter (complement s?)
	    (tree-seq s? seq x))))

(defmacro push! [coll x]
  (list 'set! coll (list 'conj coll x)))