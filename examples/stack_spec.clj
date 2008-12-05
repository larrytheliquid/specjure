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
(share-spec "non-empty specjure.examples/stack" []
  (it "is not empty"
    (ie not be empty 
      ($get :stack)))

  (it "returns the top item when applied to specjure.examples/peek"
    (ie = ($get :last-item-added) 
      (peek ($get :stack))))

  (it "does not remove the top item when applied to specjure.examples/peek"
    (ie = ($get :last-item-added) 
      (peek ($get :stack)))
    (ie = ($get :last-item-added) 
      (peek ($get :stack))))

  (it "returns the top item when applied to specjure.examples/pop!"
    (ie = ($get :last-item-added) 
      (pop! ($get :stack))))
  
  (it "removes the top item when applied to specjure.examples/pop!"
    (ie = ($get :last-item-added) 
      (pop! ($get :stack)))
    (when-not (empty? ($get :stack))
      (ie not = ($get :last-item-added) 
        (pop! ($get :stack))))))

(share-spec "non-full specjure.examples/stack" []
  (it "is not full"
    (ie not be full 
      ($get :stack)))

  (it "adds to the top when applied to specjure.examples/push!"
    (push! ($get :stack) "newly added top item")
    (when-not (empty? ($get :stack))
      (ie = "newly added top item" 
        (peek ($get :stack))))))

(spec stack
  (before ($assoc! :stack (stack)))

  (spec "(empty)"
    (it "is empty"
      (ie be empty 
        ($get :stack)))

    (use-spec "non-full specjure.examples/stack")

    (it "complains when applied to specjure.examples/peek"
      (ie throw java.util.EmptyStackException 
        (peek ($get :stack))))

    (it "complains when applied to specjure.examples/pop!"
      (ie throw java.util.EmptyStackException 
        (pop! ($get :stack)))))

  (spec "(with one item)"
    (before
      (push! ($get :stack) 3)
      ($assoc! :last-item-added 3))

    (use-spec "non-empty specjure.examples/stack")
    (use-spec "non-full specjure.examples/stack"))

  (spec "(with one item less than capactiy)"
    (before
      (doseq i (range 1 10) (push! ($get :stack) i))
      ($assoc! :last-item-added 9))

    (use-spec "non-empty specjure.examples/stack")
    (use-spec "non-full specjure.examples/stack"))

  (spec "(full)"
    (before
      (doseq i (range 1 11) (push! ($get :stack) i))
      ($assoc! :last-item-added 10))

    (it "is full"
      (ie be full 
        ($get :stack)))

    (use-spec "non-empty specjure.examples/stack")

    (it "complains when applied to specjure.examples/push!"
      (ie throw Exception 
        (push! ($get :stack) (fn []))))))