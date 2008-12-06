;;; RSpec stack example: http://rspec.info/examples.html

;;; Implementation
(ns specjure.examples 
  (:refer-clojure :exclude [empty? peek]) 
  (:use specjure))

(defn stack [] 
  (ref ()))

(defn empty? [stk] 
  (clojure.core/empty? @stk))

(defn full? [stk]
  (= (count @stk) 10))

(defn peek [stk] 
  (when (empty? stk) (throw (new java.util.EmptyStackException)))
  (clojure.core/peek @stk))

(defn push! [stk val] 
  (when (full? stk) (throw (new Exception)))
  (dosync (alter stk conj val)))

(defn pop! [stk]
  (when (empty? stk) (throw (new java.util.EmptyStackException)))
  (let [result (first @stk)]
      (dosync (alter stk pop))
      result))

(share-spec "non-empty specjure.examples/stack" []
  (spec "is not empty" (ie-not empty? ($get :stack)))

  (spec "returns the top item when applied to specjure.examples/peek"
    (ie = ($get :last-item-added) (peek ($get :stack))))

  (spec "does not remove the top item when applied to specjure.examples/peek"
    (ie = ($get :last-item-added) 
	  (do (peek ($get :stack)) (peek ($get :stack)))))

  (spec "returns the top item when applied to specjure.examples/pop!"
    (ie = ($get :last-item-added) (pop! ($get :stack))))  
  
  (spec "removes the top item when applied to specjure.examples/pop!"
    (before ($assoc! :popd (pop! ($get :stack))))

    (ie = ($get :last-item-added) ($get :popd))))

(share-spec "non-full specjure.examples/stack" []
  (spec "is not full" (ie-not full? ($get :stack)))

  (spec "adds to the top when applied to specjure.examples/push!"
    (before (push! ($get :stack) "newly added top item"))

    (ie = "newly added top item" (peek ($get :stack)))))

(spec stack
  (before ($assoc! :stack (stack)))  

  (spec "(empty)"
    (spec "is empty" (ie empty? ($get :stack)))

    (use-spec "non-full specjure.examples/stack")    

    (spec "complains when applied to specjure.examples/peek"
      (ie throws? java.util.EmptyStackException (peek ($get :stack))))
    
    (spec "complains when applied to specjure.examples/pop!"
      (ie throws? java.util.EmptyStackException (pop! ($get :stack)))))  

  (spec "(with one item)"
    (before (push! ($get :stack) 3)      
	    ($assoc! :last-item-added 3))    

    (use-spec "non-empty specjure.examples/stack")
    (use-spec "non-full specjure.examples/stack"))

  (spec "(with one item less than capactiy)"
    (before (doseq [i (range 1 10)]
	      (push! ($get :stack) i))
	    ($assoc! :last-item-added 9))    

    (use-spec "non-empty specjure.examples/stack")
    (use-spec "non-full specjure.examples/stack"))

  (spec "(full)"
    (before (doseq [i (range 1 11)]
	      (push! ($get :stack) i))      
	    ($assoc! :last-item-added 10))
    
    (spec "is full" (ie full? ($get :stack)))
    
    (use-spec "non-empty specjure.examples/stack")

    (spec "complains when applied to specjure.examples/push!"
      (ie throws? Exception (push! ($get :stack) (fn [])))))
)
