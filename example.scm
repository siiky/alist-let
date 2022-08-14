#!/usr/bin/env -S csi -s

(load "alist-let.scm")

(print "alist-ref based alist-values")

(print "Fixnum keys")
(alist-let '((0 . 1) (2 . 3) (4 . 5))
           ((a 0) (b 2) (c 4) (d 6 42))
           (print "a: " a)
           (print "b: " b)
           (print "c: " c)
           (print "d: " d))
(newline)

(print "Symbol keys")
(alist-let '((a . 1) (b . 3) (c . 5))
           ((a 'a) (b 'b) (c 'c) (d 'd 42))
           (print "a: " a)
           (print "b: " b)
           (print "c: " c)
           (print "d: " d))
(newline)

(print "String keys")
(alist-let '(("a" . 1) ("b" . 3) ("c" . 5))
           ((a "a" #f string=?) (b "b" #f string=?) (c "c" #f string=?) (d "d" 42 string=?))
           (print "a: " a)
           (print "b: " b)
           (print "c: " c)
           (print "d: " d))
(newline)


(print "key-index based alist-values")

(print "Fixnum keys")
(receive (a b c d) (alist-values/single-traverse '((0 . 1) (2 . 3) (4 . 5)) 0 2 4 6)
  (print "a: " a)
  (print "b: " b)
  (print "c: " c)
  (print "d: " d))
(newline)

(print "String keys (w/ duplicate keys)")
(receive (a1 a2 a3) (alist-values/single-traverse '(("a" . 1) ("a" . 2) ("b" . 4)) "a" "a" "a")
  (print "a1: " a1)
  (print "a2: " a2)
  (print "a3: " a3))
(newline)
