;;; Data structures
(def *failed-expectations*)
(defstruct example :description :behavior)
(defstruct expectation :expected :actual)

;;; Data structure modifiers
(def describe-fn-map
     {:description (fn [new-value old-value] (str new-value " " old-value))
      :behavior (fn [new-value old-value] (new-value old-value))})

(defn describe-example [options example]
  (apply assoc example (mapcat (fn [option] [option ((option describe-fn-map) 
						     (option options) 
						     (option example))])
			       (keys options))))

;;; Public interface
(defmacro describe [options & body]
  `(let [options# (if (string? ~options) {:description ~options} ~options)]
     (map (fn [example#] (describe-example options# example#)) 
	  (flatten (list ~@body)))))

(defmacro it [description & behavior]
  `(struct example ~description '(binding [*failed-expectations* []]
					 ~@behavior
					 *failed-expectations*)))

(defmacro => [expected should matcher actual]
  `(let [expectation# (struct expectation ~expected ~actual)]
     (when-not (passed? expectation#)
       (push! *failed-expectations* expectation#))))

(defmacro check-examples [& body]
  `(let [examples# (map check (concat ~@body))
	 examples-count# (count examples#)
	 failures-count# (count (filter :failed-expectations examples#))]
     (printf "%n%s Examples, %s Failures%n" examples-count# failures-count#)))

;;; Verification
(defn check [example]
  (let [failed-expectations (eval (:behavior example))
	example-passed? (empty? failed-expectations)]
    (printf "%s%s%n" (:description example) (if example-passed? "" " (FAILED)"))
    (if example-passed? example 
	(assoc example :failed-expectations failed-expectations))))

(defn passed? [expectation]
  (= (:expected expectation) (:actual expectation)))

;;; Utilities
(defn flatten [x]
  (let [s? #(instance? clojure.lang.Sequential %)]
    (filter (complement s?)
	    (tree-seq s? seq x))))

(defmacro push! [coll x]
  (list 'set! coll (list 'conj coll x)))