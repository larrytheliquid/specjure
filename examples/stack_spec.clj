;; RSpec stack example: http://rspec.info/examples.html
(ns example (:use specjure))

(defn stack [])
(defn full? [stk])
(defn push! [stk val])
(defn pop! [stk])

(shared-examples-for "non-empty example/stack" []
  (it "is not empty"
    (should not = true (empty? (param :stack))))

  (it "returns the top item when applied to example/peek"
    (should = (param :last-item-added) (peek (param :stack))))

  (it "does not remove the top item when applied to example/peek"
    (should = (param :last-item-added) (peek (param :stack)))
    (should = (param :last-item-added) (peek (param :stack))))

  (it "returns the top item when applied to example/pop!"
    (should = (param :last-item-added) (pop! (param :stack))))
  
  (it "removes the top item when applied to example/pop!"
    (should = (param :last-item-added) (pop! (param :stack)))
    (when-not (empty? (param :stack))
      (should not = (param :last-item-added) (pop! (param :stack))))))

(shared-examples-for "non-full example/stack" []
  (it "is not full"
    (should not = true (full? (param :stack))))

  (it "adds to the top when applied to example/push!"
    (push! (param :stack) "newly added top item")
    (when-not (empty? (param :stack))
      (should not = "newly added top item" (peek (param :stack))))))

(describe stack
  (before-each (set-param :stack (stack)))

  (describe "(empty)"
    (it "is empty"
      (should = true (empty? (param :stack))))

    (it-should-behave-like "non-full example/stack")

    (it "complains when applied to example/peek")

    (it "complains when applied to example/pop!"))

  (describe "(with one item)"
    (before-each
      (push! (param :stack) 3)
      (set-param :last-item-added 3))

    (it-should-behave-like "non-empty example/stack")
    (it-should-behave-like "non-full example/stack"))

  (describe "(with one item less than capactiy)"
    (before-each
      (doseq i (range 1 10) (push! (param :stack) i))
      (set-param :last-item-added 10)))

  (it "is full"
    (should = true (full? (param :stack))))

  (it-should-behave-like "non-empty example/stack")

  (it "complains when applied to example/push!"))