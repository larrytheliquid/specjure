;;; Data structures
(def *example-groups*)
(def *failed-expectations*)
(defstruct example :description :behavior)
(defstruct expectation :expected :actual)

;;; Public interface
(defmacro describe [description & body]
  `(map (fn [example#]
	  (assoc example# :description (str ~description " " (:description example#))))
	(flatten (list ~@body))))

(defmacro it [description & behavior]
  `(struct example ~description (fn [] (binding [*failed-expectations* []]
					 ~@behavior
					 *failed-expectations*))))

(defmacro => [expected should matcher actual]
  `(let [expectation# (struct expectation ~expected ~actual)]
     (when-not (passed? expectation#)
       (push! *failed-expectations* expectation#))))

(defmacro run-examples [& body]
  `(let [examples# (map run (concat ~@body))
	 examples-count# (count examples#)
	 failures-count# (count (filter :failed-expectations examples#))]
     (printf "%n%s Examples, %s Failures%n" examples-count# failures-count#)))

;;; Verification
(defn run [example]
  (let [failed-expectations ((:behavior example)) example-passed? (empty? failed-expectations)]
    (printf "%s%s%n" (:description example) (if example-passed? "" " (FAILED)"))
    (if example-passed? example (assoc example :failed-expectations failed-expectations))))

(defn passed? [expectation]
  (= (:expected expectation) (:actual expectation)))

;;; Utilities
(defn flatten [x]
  (let [s? #(instance? clojure.lang.Sequential %)]
    (filter (complement s?)
	    (tree-seq s? seq x))))

(defmacro push! [coll x]
  (list 'set! coll (list 'conj coll x)))