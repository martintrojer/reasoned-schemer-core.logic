(ns reasoned-schemer.ch10
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

(run* [x]
      (conda
        [(== :olive x) s#]
        [(== :oil x) s#]))

(run* [x]
      (conde
        [(== :olive x) s#]
        [(== :oil x) s#]))

(run* [x]
      (conda
        [(== :virgin x) u#]
        [(== :olive x) s#]
        [(== :oil x) s#]))

(run* [q]
      (fresh [x y]
             (== :split x)
             (== :pea y)
             (conda
               [(== :split x) (== x y)]
               [s#]))
      (== true q))

(run* [q]
      (fresh [x y]
             (== :split x)
             (== :pea y)
             (conda
               [(== x y) (== :split x)]
               [s#]))
      (== true q))

;; ---

(defn not-pastao [x]
  (conda
    [(== :pasta x) u#]
    [s#]))

(run* [x]
      (conda
        [(not-pastao x) u#]
        [(== :spagetti x)]))

(run* [x]
      (== :spagetti x)
      (conda
        [(not-pastao x) u#]
        [(== :spagetti x)]))

;; --

(defn anyo [g]
  (conde
    [g s#]
    [(anyo g)]))

(def alwayso (anyo s#))
(def nevero (anyo u#))

(run 1 [q]
     (conda
       [alwayso s#]
       [u#])
     (== true q))

(run* [q]
      (condu
        [alwayso s#]
        [u#])
      (== true q))

(run 1 [q]
      (condu
        [alwayso s#]
        [u#])
      u#
      (== true q))

;; ---

(defn onceo [g]
  (condu
    [g s#]
    [u#]))

(defn teacupo [x]
  (conde
    ((== :tea x) s#)
    ((== :cup x) s#)))

(run* [x]
      (onceo (teacupo x)))

(defn salo [g]
  (conde
    [s#]
    [g]))

(run 1 [q]
     (onceo (salo nevero))
     u#)

;; ---

(defn bumpo [n x]
  (conde
    [(== n x) s#]
    [(fresh [m]
            (reasoned-schemer.ch7/-o n '(1) m)
            (bumpo m x))]))

(run* [x]
      (bumpo '(1 1 1) x))

(defn gen&testo [op i j k]
  (onceo
   (fresh [x y z]
          (op x y z)
          (== i x)
          (== j y)
          (== k z))))

(run* [q]
      (gen&testo reasoned-schemer.ch7/+o '(0 0 1) '(1 1) '(1 1 1))
      (== true q))

;; ---

(defn enumerateo [op r n]
  (fresh [i j k]
         (bumpo n i)
         (bumpo n j)
         (op i j k)
         (gen&testo op i j k)
         (== [i j k] r)))

(run* [s]
      (enumerateo reasoned-schemer.ch7/+o s '(1 1)))

