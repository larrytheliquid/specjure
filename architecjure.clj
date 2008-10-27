;;; Data structures
(def *failed-expectations*)
(defstruct requirement :description :behavior)
(defstruct expectation :expected :actual)

;;; Data structure modifiers
(def architect-fn-map
     {:description (fn [new old] (str new " " old))})

(defn architect-requirement [options requirement]
  (apply assoc requirement (mapcat (fn [option] [option ((option architect-fn-map) 
						     (option options) 
						     (option requirement))])
			       (keys options))))

;;; Public interface
(defmacro architect [options & body]
  `(let [options# (if (string? ~options) {:description ~options} ~options)]
     (map (fn [requirement#] (architect-requirement options# requirement#)) 
	  (flatten (list ~@body)))))

(defmacro it [description & behavior]
  `(struct requirement ~description (fn [] (binding [*failed-expectations* []]
					 ~@behavior
					 *failed-expectations*))))

(defmacro => [expected should matcher actual]
  `(let [expectation# (struct expectation ~expected ~actual)]
     (when-not (satisfied? expectation#)
       (push! *failed-expectations* expectation#))))

(defmacro verify-requirements [& body]
  `(let [requirements# (map verify (concat ~@body))
	 requirements-count# (count requirements#)
	 failures-count# (count (filter :failed-expectations requirements#))]
     (printf "%n%s Requirements, %s Failures%n" requirements-count# failures-count#)))

;;; Verification
(defn verify [requirement]
  (let [failed-expectations ((:behavior requirement)) 
	requirement-satisfied? (empty? failed-expectations)]
    (printf "%s%s%n" (:description requirement) (if requirement-satisfied? "" " (FAILED)"))
    (if requirement-satisfied? requirement 
	(assoc requirement :failed-expectations failed-expectations))))

(defn satisfied? [expectation]
  (= (:expected expectation) (:actual expectation)))

;;; Utilities
(defn flatten [x]
  (let [s? #(instance? clojure.lang.Sequential %)]
    (filter (complement s?)
	    (tree-seq s? seq x))))

(defmacro push! [coll x]
  (list 'set! coll (list 'conj coll x)))