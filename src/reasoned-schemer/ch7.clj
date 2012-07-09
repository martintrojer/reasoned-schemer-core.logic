(ns reasoned-schemer.ch7
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

(defn bit-nando [x y r]
  (conde
    [(== 0 x) (== 0 y) (== 1 r)]
    [(== 1 x) (== 0 y) (== 1 r)]
    [(== 0 x) (== 1 y) (== 1 r)]
    [(== 1 x) (== 1 y) (== 0 r)]
    [u#]))

(defn bit-xoro [x y r]
  (fresh [s t u]
         (bit-nando x y s)
         (bit-nando x s t)
         (bit-nando s y u)
         (bit-nando t u r)))

(run* [s]
      (fresh [x y z]
             (bit-xoro x y z)
             (== [z x y] s)))

(defn bit-noto [x r]
  (bit-nando x x r))

(defn bit-ando [x y r]
  (fresh [s]
         (bit-nando x y s)
         (bit-noto r s)))

(run* [s]
      (fresh [x y]
             (bit-ando x y 1)
             (== [x y] s)))

;; ---

(defn half-addero [x y r c]
  (all
   (bit-xoro x y r)
   (bit-ando x y c)))

(run* [r]
      (half-addero 1 1 r 1))

(run* [s]
      (fresh [x y r c]
             (half-addero x y r c)
             (== [x y r c] s)))

(defn full-addero [b x y r c]
  (fresh [w xy wz]
         (half-addero x y w xy)
         (half-addero w b r wz)
         (bit-xoro xy wz c)))

(run* [s]
      (fresh [r c]
             (full-addero 0 1 1 r c)
             (== [r c] s)))

(run* [s]
      (fresh [b x y r c]
             (full-addero b x y r c)
             (== [b x y r c] s)))

;; ---

(defn build-num [n]
  (cond
    (zero? n) '()
    (even? n) (cons 0 (build-num (/ n 2)))
    :else (cons 1 (build-num (/ (dec n) 2)))))

(build-num 0)
(build-num 17290)

;; non-overlapping property version (any order will do)

(defn build-num2 [n]
  (cond
    (zero? n) '()
    (and (even? n) (not (zero? n))) (cons 0 (build-num (/ n 2)))
    (odd? n) (cons 1 (build-num (/ (dec n) 2)))))

(build-num2 36)

;; ---

(defn poso [n]
  (fresh [a d]
         (== (llist a d) n)))

(run* [q]
      (poso '(1 1 0))
      (== true q))

(run* [q]
      (poso '())
      (== true q))

(run 1 [q]
      (poso q))

(defn >1o [n]
  (fresh [a ad dd]
         (== (llist a ad dd) n)))

(run* [q]
      (>1o '(0 1))
      (== true q))

(run* [q]
      (>1o '(1))
      (== true q))

;; ---

(defn width [n]
  (cond (empty? n) 0
        (coll? n) (inc (width (rest n)))
        :else 1))

(width (build-num 32))

(declare gen-addero)

(defn addero [d n m r]
  (conde
   [(== 0 d) (== '() m) (== n r)]
   [(== 0 d) (== '() n) (== m r) (poso m)]
   [(== 1 d) (== '() m) (addero 0 n '(1) r)]
   [(== 1 d) (== '() n) (poso m) (addero 0 '(1) m r)]
   [(== '(1) n) (== '(1) m) (fresh [a c]
                                   (== (list a c) r)
                                   (full-addero d 1 1 a c))]
   [(== '(1) n) (gen-addero d n m r)]
   [(== '(1) m) (>1o n) (>1o r) (addero d '(1) n r)]
   [(>1o n) (gen-addero d n m r)]))

(defn gen-addero [d n m r]
  (fresh [a b c e x y z]
         (== (llist a x) n)
         (== (llist b y) m) (poso y)
         (== (llist c z) r) (poso z)
         (all
          (full-addero d a b c e)
          (addero e x y z))))

(run* (s)
      (gen-addero 1 '(0 1 1) '(1 1) s))

(run* (s)
      (fresh [x y]
             (addero 0 x y '(1 0 1))
             (== [x y] s)))

(defn +o [n m k]
  (addero 0 n m k))

(run* [s]
      (fresh [x y]
             (+o x y '(1 0 1))
             (== [x y] s)))

(defn -o [n m k]
  (+o m k n))

(run* [q]
      (-o '(0 0 0 1) '(1 0 1) q))

(run* [q]
      (-o '(0 1 1) '(0 1 1) q))
