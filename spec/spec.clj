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

(load-file "specjure.clj")
(load-file "spec/describe/default-spec.clj")
(load-file "spec/describe/before-spec.clj")
(load-file "spec/describe/integration-spec.clj")
(load-file "spec/should/equal-spec.clj")
(load-file "spec/should/be-predicate-spec.clj")

(specjure/check-examples)