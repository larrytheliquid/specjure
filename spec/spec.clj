;;; TODO: Maybe break up files into a lib directory
;;; TODO: Make appropriate functions private
;;; TODO: Use correct optional parameter syntax (type defn in emacs for an example)
;;; TODO: Mandator list second parameter, and mandator string first paramater
;;; TODO: Support for vector/list versions of before, after, before-each, and after-each
;;; TODO: Add clojure namespace functions to "be" predicate matchers
;;; TODO: should be-close
;;; TODO: README.markdown
;;; TODO: Documentation, especially for describe
;;; TODO: Symbol-support in describe names
;;; TODO: Raise error with nice message when not giving correct macro syntax, or using a non-existent option
;;; TODO: Implement describes dispatch with multimethods
;;; TODO: Use Exceptions for expectation failures
;;; TODO: Implement function versions of all macros for them to expand to
;;; TODO: Run examples function that takes a directory or file as input
;;; TODO: Reorganize specs to meet new requirements
;;; TODO: Examples directory, including the RSpec stack example
;;; TODO: Shared examples
;;; TODO: Consider pattern-matching matcher
;;; TODO: Consider shared examples for 3 number types
;;; TODO: Consider shared examples for vector/list/map [:one, :two] & [1, 2] for a map
;;; TODO: Consider storing spec docs and/or functions in meta data of real functions
;;; TODO: Different reporting for pending, success, failure, exception, and expectation
;;; TODO: Parallel check-examples
;;; TODO: Document how to extend with custom options

(load-file "specjure.clj")
(load-file "spec/describe-spec.clj")
(load-file "spec/before-spec.clj")
(load-file "spec/should-spec.clj")
(load-file "spec/should-not-spec.clj")

(specjure/check-examples)