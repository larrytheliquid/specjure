;;; RSpec stack example: http://rspec.info/examples.html

;;; Implementation
(ns specjure.examples 
  (:refer-clojure :exclude [empty? peek]) 
  (:use specjure))

(defn stack [] 
  (ref ()))

(defn empty? [stk] 
  (clojure/empty? @stk))

(defn full? [stk]
  (= (count @stk) 10))

(defn peek [stk] 
  (when (empty? stk) (throw (new java.util.EmptyStackException)))
  (clojure/peek @stk))

(defn push! [stk val] 
  (when (full? stk) (throw (new Exception)))
  (dosync (alter stk conj val)))

(defn pop! [stk]
  (when (empty? stk) (throw (new java.util.EmptyStackException)))
  (let [result (first @stk)]
      (dosync (alter stk pop))
      result))

;;; Specs
(shared-examples-for "non-empty specjure.examples/stack" []
  (it "is not empty"
    (should not be empty ($get :stack)))

  (it "returns the top item when applied to specjure.examples/peek"
    (should = ($get :last-item-added) (peek ($get :stack))))

  (it "does not remove the top item when applied to specjure.examples/peek"
    (should = ($get :last-item-added) (peek ($get :stack)))
    (should = ($get :last-item-added) (peek ($get :stack))))

  (it "returns the top item when applied to specjure.examples/pop!"
    (should = ($get :last-item-added) (pop! ($get :stack))))
  
  (it "removes the top item when applied to specjure.examples/pop!"
    (should = ($get :last-item-added) (pop! ($get :stack)))
    (when-not (empty? ($get :stack))
      (should not = ($get :last-item-added) (pop! ($get :stack))))))

(shared-examples-for "non-full specjure.examples/stack" []
  (it "is not full"
    (should not be full ($get :stack)))

  (it "adds to the top when applied to specjure.examples/push!"
    (push! ($get :stack) "newly added top item")
    (when-not (empty? ($get :stack))
      (should = "newly added top item" (peek ($get :stack))))))

(describe stack
  (before-each ($assoc! :stack (stack)))

  (describe "(empty)"
    (it "is empty"
      (should be empty ($get :stack)))

    (it-should-behave-like "non-full specjure.examples/stack")

    (it "complains when applied to specjure.examples/peek"
      (should throw java.util.EmptyStackException 
	(peek ($get :stack))))

    (it "complains when applied to specjure.examples/pop!"
      (should throw java.util.EmptyStackException 
	(pop! ($get :stack)))))

  (describe "(with one item)"
    (before-each
      (push! ($get :stack) 3)
      ($assoc! :last-item-added 3))

    (it-should-behave-like "non-empty specjure.examples/stack")
    (it-should-behave-like "non-full specjure.examples/stack"))

  (describe "(with one item less than capactiy)"
    (before-each
      (doseq i (range 1 10) (push! ($get :stack) i))
      ($assoc! :last-item-added 9))

    (it-should-behave-like "non-empty specjure.examples/stack")
    (it-should-behave-like "non-full specjure.examples/stack"))

  (describe "(full)"
    (before-each
      (doseq i (range 1 11) (push! ($get :stack) i))
      ($assoc! :last-item-added 10))

    (it "is full"
      (should be full ($get :stack)))

    (it-should-behave-like "non-empty specjure.examples/stack")

    (it "complains when applied to specjure.examples/push!"
      (should throw Exception 
	(push! ($get :stack) (fn []))))))