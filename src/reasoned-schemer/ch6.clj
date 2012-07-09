(ns reasoned-schemer.ch6
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

;; putting some more core.logic optimizations in here...

(defn anyo [g]
  (conde
    [g s#]
    [(anyo g)]))

(def nevero (anyo u#))

(run 1 [q]
     u#
     nevero)

(def alwayso (anyo s#))

(run 5 [q]
     alwayso
     (== true q))

(defn salo [g]
  (conde
    [s#]
    [g]))

(run 1 [q]
     (salo alwayso)
     (== true q))

(run 1 [q]
     (salo nevero)
     (== true q))

(run 1 [q]
     (conde                     ;; core.logic conde == rs condi
      [(== false q) alwayso]
      [(== true q)])
     (== true q))

(run 2 [q]
     (conde
       [(== false q) alwayso]
       [(anyo (== true q))])
     (== true q))

;; ---

(defn teacupo [x]
  (conde
    [(== :tea x) s#]
    [(== :cup x) s#]))

(run* [x]
      (teacupo x))

(run 5 [r]
     (conde
       [(teacupo r) s#]
       [(== false r) s#]
       [u#]))

(run 1 [q]
     (all
      (conde
        [(== false q) s#]
        [(== true q)])
      alwayso)
     (== true q))

(run 5 [q]
     (all
      (conde
        [s# s#]
        [nevero])
      alwayso)
     (== true q))
