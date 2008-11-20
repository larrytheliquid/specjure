(ns specjure)

;;; Utilities
(defn flatten [x]
  (let [s? #(instance? clojure.lang.Sequential %)]
    (filter (complement s?)
	    (tree-seq s? seq x))))

(defmacro push! [coll x]
  (list 'set! coll (list 'conj coll x)))

;;; Data structures
(def *examples* (agent []))
(def *failed-expectations*)
(defstruct example :description :behavior)
(defstruct expectation :comparator :expected :actual)

;;; Verification
(defn check [example]
  (let [failed-expectations ((:behavior example))
	example-passed? (empty? failed-expectations)]
    (printf "%s%s%n" (:description example) (if example-passed? "" " (FAILED)"))
    (if example-passed? example 
	(assoc example :failed-expectations failed-expectations))))

(defn passed? [expectation]
  ((:comparator expectation) (:expected expectation) (:actual expectation)))

;;; Public interface
(defmacro describe [desc & body]
  `(map (fn [example#] 
	  (assoc example# :description (str ~desc " " (:description example#)))) 
	(flatten (list ~@body))))

(defmacro describe-let [desc options & body]
  `(let [~@options] (describe ~desc ~@body)))

(defmacro it [description & behavior]
  `(let [example#
	 (struct example ~description #(binding [*failed-expectations* []]
						~@behavior
						*failed-expectations*))]
     (send *examples* conj example#)
     example#))

(defmacro should [matcher & arguments]
  `(let [expectation# (apply struct expectation = (parse-matcher '~matcher ~@arguments))]
     (when-not (passed? expectation#)
       (push! *failed-expectations* expectation#))))

(defmacro should-not [matcher & arguments]
  `(let [expectation# (apply struct expectation (complement =) (parse-matcher '~matcher ~@arguments))]
     (when-not (passed? expectation#)
       (push! *failed-expectations* expectation#))))

(defn be-function [be-matcher]
  (resolve (symbol (apply str 
			  (conj (vec (drop 3 (str be-matcher))) \?)))))

(defn parse-matcher [matcher & arguments]
  (cond (= matcher '=) arguments
	(= (symbol (apply str (take 3 (str matcher))))  'be-) 
	[(apply (be-function matcher)  arguments) true]))

(defn check-examples 
  ([] (await *examples*) (check-examples @*examples*))
  ([body]
     (send *examples* (fn [_] []))
     (let [examples (map check body)
	   examples-count (count examples)
	   failures-count (count (filter :failed-expectations examples))]
       (printf "%n%s Examples, %s Failures%n" examples-count failures-count)
       (doseq failed-example (filter :failed-expectations examples)
	 (doseq failed-expectation (:failed-expectations failed-example)
	   (printf "%n'%s' FAILED%nexpected: %s%ngot: %s (using =)%n" 
		   (:description failed-example)
		   (:expected failed-expectation)
		   (:actual failed-expectation)))))))

;; (defmacro check-examples 
;;   ([] `(do @*examples*))
;;   ([body]
;;   `(let [examples# (map check (concat ~@body))
;; 	 examples-count# (count examples#)
;; 	 failures-count# (count (filter :failed-expectations examples#))]
;;      (printf "%n%s Examples, %s Failures%n" examples-count# failures-count#)
;;      (doseq failed-example# (filter :failed-expectations examples#)
;;        (doseq failed-expectation# (:failed-expectations failed-example#)
;; 	 (printf "%n'%s' FAILED%nexpected: %s%ngot: %s (using =)%n" 
;; 	       (:description failed-example#)
;; 	       (:expected failed-expectation#)
;; 	       (:actual failed-expectation#)))))))