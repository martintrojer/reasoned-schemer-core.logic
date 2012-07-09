(ns reasoned-schemer.ch4
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

(defn eq-caro [l x]
  (firsto l x))

(defn memo [x l out]      ;; not a boolean function needs out param
  (conde
   ((eq-caro l x) (== l out))
   ((s# (fresh (d)
               (resto l d)
               (memo x d out))))))

(run* (out)
      (memo :tofu [:a :b :tofu :d :tofu "e"] out))

(run 1 (out)   ;; !!!
     (fresh (x)
            (memo :tofu [:a :b x :d :tofu "e"] out)))

(run* (r)
      (memo r [:a :b :tofu :d :tofu "e"] [:tofu :d :tofu "e"]))

(run* (q)
      (memo :tofu [:tofu :e] [:tofu :e])
      (== true q))

(run* (q)
      (memo :tofu [:tofu :e] [:tofu])
      (== true q))

;; ---

(defn rembero [x l out]
  (conde
   ((emptyo l) (== out []))
   ((eq-caro l x) (resto l out))
   (s# (fresh (res)
              (fresh (d)
                     (resto l d)
                     (rembero x d res))
              (fresh (a)
                     (resto l a)
                     (conso a res out))))))

(defn rembero [x l out]
  (conde
   ((emptyo l) (== out []))
   ((eq-caro l x) (resto l out))
   (s# (fresh (a d res)
              (resto l d)
              (rembero x d res)
              (firsto l a)
              (conso a res out)))))

(defn rembero [x l out]
  (conde
   ((emptyo l) (== out []))
   ((eq-caro l x) (resto l out))
   (s# (fresh (a d res)
              (conso a d l)
              (rembero x d res)
              (conso a res out)))))

(run* (out)
     (fresh (y res)
            (rembero :peas [:a :b y :d :peas :e] res)
            (== out [y res])))

(run* (out)
      (fresh (y z res)
             (rembero y [:a :b y :d z :e] res)
             (== out [y z res])))

(run* (r)
      (fresh (y z)
             (rembero y [y :d z :e] [y :d :e])
             (== [y z] r)))

(run 13 (w)
     (fresh (y z out)
            (rembero y (llist :a :b y :d z w) out)))

;; ---

(defn surpriseo [s]
  (rembero s [:a :b :c] [:a :b :c]))

(run* (r)
     (surpriseo r))

(run* (r)               ;; wtf!!!
      (surpriseo r)
      (== :b r))
