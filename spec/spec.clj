;;; TODO: Make appropriate functions private
;;; TODO: Support for vector/list versions of before, after, before-each, and after-each
;;; TODO: Add clojure namespace functions to "be" predicate matchers
;;; TODO: should be-close
;;; TODO: README.markdown
;;; TODO: Documentation, especially for describe
;;; TODO: Use Exceptions for expectation failures
;;; TODO: Examples directory, including the RSpec stack example
;;; TODO: Shared examples
;;; TODO: Consider pattern-matching matcher
;;; TODO: Consider shared examples for 3 number types
;;; TODO: Consider shared examples for vector/list/map [:one, :two] & [1, 2] for a map
;;; TODO: Consider storing spec docs and/or functions in meta data of real functions
;;; TODO: Different reporting for pending, success, failure, exception, and expectation
;;; TODO: Parallel check-examples
;;; TODO: Add file and line number to failing spec information
;;; TODO: A sync-before, wrapping the before in dosync so you can use its body

(load-file "specjure.clj")
(load-file "spec/describe/default_spec.clj")
;; (load-file "spec/describe/before_spec.clj")
;; (load-file "spec/describe/integration_spec.clj")
(load-file "spec/should/equal_spec.clj")
(load-file "spec/should/be_true_spec.clj")
(load-file "spec/should/be_false_spec.clj")

(specjure/check-examples)