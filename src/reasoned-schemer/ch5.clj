(ns reasoned-schemer.ch5
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

(defn appendo2 [l s out]
  (conde
    ((emptyo l) (==  s out))
    (s# (fresh (a d res)
               (firsto l a)
               (resto l d)
               (appendo2 d s res)
               (conso a res out)))))

(run* (x)
      (appendo2 [:cake] [:tastes :yummy] x))

(run* (x)
      (fresh (y)             
             (appendo2 [:cake :with :ice y]
                       [:tastes :yummy]
                       x)))

(run* (x)
      (fresh (y)
             (appendo2 [:cake :with :ice :cream]
                       y x)))

(run 5 (x)
     (fresh (y)
            (appendo2 (llist :case :with :ice y)
                      [:d :t] x)))

(run 5 (x)
     (fresh (y)
            (appendo2 (llist :case :wit :ice y)
                      (llist :d :t y)
                      x)))

(defn appendo3 [l s out]
  (conde
    ((emptyo l) (==  s out))
    (s# (fresh (a d res)
               (conso a d l)
               (appendo3 d s res)
               (conso a res out)))))

(run* (x)
      (appendo3 [:cake] [:tastes :yummy] x))

(run 6 (r)
     (fresh (x y)
            (appendo3 x y [:cake :with :ice :d :t])
            (== [x y] r)))

(defn appendo4 [l s out]
  (conde
    ((emptyo l) (==  s out))
    (s# (fresh (a d res)
               (conso a d l)
               (conso a res out)
               (appendo4 d s res)))))

(run 7 (r)
     (fresh (x y)
            (appendo4 x y [:cake :with :ice :d :t])
            (== [x y] r)))

(run 7 (r)
     (fresh (x y z)
            (appendo4 x y z)
            (== [x y z] r)))

(defn swappendo [l s out]
  (conde
    (s# (fresh (a d res)
               (conso a d l)
               (conso a res out)
               (appendo4 d s res)))
    ((emptyo l) (==  s out))))

(run* (x)
      (fresh (y)
             (swappendo [:cake] [:tastes :yummy y] x)))

;; ---

(defn pairo [p]
  (fresh [a d]
    (== (lcons a d) p)))

(defn unwrapo [x out]
  (conde
    ((pairo x) (fresh (a)
                      (firsto x a)
                      (unwrapo a out)))
    ((s# (== out x)))))

(run* (x)
      (unwrapo [[[[:pizza]]]] x))

(run 1 (x)
     (unwrapo x :pizza))

(run 1 (x)
     (unwrapo [[x]] :pizza))

(defn unwrapo2 [x out]
  (conde
    ((s# (== out x)))
    ((s#) (fresh (a)
                 (firsto x a)
                 (unwrapo a out)))))

(run 5 (x)
     (unwrapo x :pizza))

(run 5 (x)
     (unwrapo [[x]] :pizza))

;; ----

(defn flatten [s]
  (cond
    (nil? s) '()
    (coll? s) (if (empty? s) '()
                  (concat (flatten (first s))
                          (flatten (rest s))))
    :else (list s)))

(flatten [[:a :b] :c])

(defn flatteno [s out]
  (conde
    ((emptyo s) (== out '()))
    ((pairo s) (fresh (a d x y)
                      (firsto s a)
                      (resto s d)
                      (flatteno a x)
                      (flatteno d y)
                      (appendo x y out)))
    (s# (conso s '() out))))

(run 10 (x)
     (flatteno [[:a :b] :c] x))        ;; the order is different from mK

(run* (x)
      (flatteno '(:a) x))

(defn flattenrevo [s out]
  (conde
    (s# (conso s '() out))
    ((emptyo s) (== out '()))    
    (s# (fresh (a d x y)
               (conso a d s)
               (flatteno a x)
               (flatteno d y)
               (appendo x y out)))))

(run* (x)
      (flattenrevo [[:a :b] :c] x))

(run 2 [x]
     (flattenrevo x [:a :b :c]))

(count
 (run* (x)
       (flattenrevo [[[[:a [[[:b]]] :c]]] :d] x)))
