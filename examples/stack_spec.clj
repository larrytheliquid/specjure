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
    (should not be empty (param :stack)))

  (it "returns the top item when applied to specjure.examples/peek"
    (should = (param :last-item-added) (peek (param :stack))))

  (it "does not remove the top item when applied to specjure.examples/peek"
    (should = (param :last-item-added) (peek (param :stack)))
    (should = (param :last-item-added) (peek (param :stack))))

  (it "returns the top item when applied to specjure.examples/pop!"
    (should = (param :last-item-added) (pop! (param :stack))))
  
  (it "removes the top item when applied to specjure.examples/pop!"
    (should = (param :last-item-added) (pop! (param :stack)))
    (when-not (empty? (param :stack))
      (should not = (param :last-item-added) (pop! (param :stack))))))

(shared-examples-for "non-full specjure.examples/stack" []
  (it "is not full"
    (should not be full (param :stack)))

  (it "adds to the top when applied to specjure.examples/push!"
    (push! (param :stack) "newly added top item")
    (when-not (empty? (param :stack))
      (should = "newly added top item" (peek (param :stack))))))

(describe stack
  (before-each (set-param :stack (stack)))

  (describe "(empty)"
    (it "is empty"
      (should be empty (param :stack)))

    (it-should-behave-like "non-full specjure.examples/stack")

    (it "complains when applied to specjure.examples/peek"
      (should throw java.util.EmptyStackException 
	(peek (param :stack))))

    (it "complains when applied to specjure.examples/pop!"
      (should throw java.util.EmptyStackException 
	(pop! (param :stack)))))

  (describe "(with one item)"
    (before-each
      (push! (param :stack) 3)
      (set-param :last-item-added 3))

    (it-should-behave-like "non-empty specjure.examples/stack")
    (it-should-behave-like "non-full specjure.examples/stack"))

  (describe "(with one item less than capactiy)"
    (before-each
      (doseq i (range 1 10) (push! (param :stack) i))
      (set-param :last-item-added 9))

    (it-should-behave-like "non-empty specjure.examples/stack")
    (it-should-behave-like "non-full specjure.examples/stack"))

  (describe "(full)"
    (before-each
      (doseq i (range 1 11) (push! (param :stack) i))
      (set-param :last-item-added 10))

    (it "is full"
      (should be full (param :stack)))

    (it-should-behave-like "non-empty specjure.examples/stack")

    (it "complains when applied to specjure.examples/push!"
      (should throw Exception 
	(push! (param :stack) (fn []))))))