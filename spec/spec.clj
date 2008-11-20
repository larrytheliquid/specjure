(load-file "specjure.clj")
(load-file "spec/describe-spec.clj")
(load-file "spec/describe-let-spec.clj")
(load-file "spec/should-spec.clj")
(load-file "spec/should-not-spec.clj")

(specjure/check-examples)