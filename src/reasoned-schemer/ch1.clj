(ns reasoned-schemer.ch1
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

u#
s#

(run* (q) u#)
(run* (q) s#)

(run* (q) (== true q))

(run* (q)
      u#
      (== true q))

(run* (r)
      s#
      (== :corn r))

(run* (x)
      (let [x false]
        (== true x)))

(run* (q)
      (fresh (x)
             (== true x)
             (== true q)))
(run* (q)
      (fresh (x)
             (== true x)
             (== x q)))

(run* (x)
      (let [x false]
        (fresh (x)
               (== true x))))

(run* (r)
      (fresh (x y)
             (== (cons x (cons y '())) r)))

(run* (r)
      (fresh (x y)
             (== (conj [] x y) r)))

(run* (r)
      (fresh (x)
             (let [y x]
               (fresh (x)
                      (== (cons y (cons x [y])) r)))))

(run* (q)
      (fresh (x)
             (== x q)
             (== true x)))

;; ---

(run* (x)
      (conde
        ((== :olive x) s#)
        ((== :oil x) s#)
        (:else u#)))            ;; "not supported", conde stmts that are guaranteed to fail is not needed

(run 1 (x)
      (conde
        ((== :olive x) s#)
        ((== :oil x) s#)))

(run* (x)
      (conde
        ((== :virgin x) u#)
        ((== :olive x) s#)
        (s# s#)
        ((== :oil x) s#)))

(run 2 (x)
      (conde
        ((== :extra x) s#)
        ((== :virgin x) u#)
        ((== :olive x) s#)
        (s# s#)
        ((== :oil x) s#)))

(run* (r)
      (fresh (x y)
             (== :split x)
             (== :pea y)
             (== (cons x [y]) r)))

(run* (r)
      (fresh (x y)
             (conde               
               ((== :split x) (== :pea y))
               ((== :navy x) (== :bean y)))
             (== (cons x [y]) r)))

(run* (r)
      (fresh (x y)
             (conde               
               ((== :split x) (== :pea y))
               ((== :navy x) (== :bean y)))
             (== (conj '(:soup) x y) r)))

;; ----

(defn teacupo [x]
  (conde
    ((== :tea x) s#)
    ((== :cup x) s#)))

(run* (x)
      (teacupo x))

(run* (r)
      (fresh (x y)
             (conde
               ((teacupo x) (== true y) s#)
               ((== false x) (== true y)))
             (== (cons x [y]) r)))

(run* (r)
      (fresh (x y z)
             (conde
               ((== y x) (fresh (x) (== z x)))
               ((fresh (x) (== y x)) (== z x)))
             (== (cons y [z]) r)))

(run* (r)
      (fresh (x y z)
             (conde
               ((== y x) (fresh (x) (== z x)))
               ((fresh (x) (== y x)) (== z x)))
             (== false x)
             (== (cons y [z]) r)))

(run* (q)
      (let [a (== true q)
            b (== false q)]        
        b))