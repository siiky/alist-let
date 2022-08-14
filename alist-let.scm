(import srfi-1 srfi-8)

(define (alist-delete-first key alist =?)
  (cond
    ((null? alist)
     '())
    ((=? key (caar alist))
     (cdr alist))
    (else
      (cons (car alist)
            (alist-delete-first key (cdr alist) =?)))))

(define (alist-values/single-traverse al key-indices vec =?)
  (if (or (null? al) (null? key-indices))
      (apply values (vector->list vec))
      (let* ((key (caar al))
             (val (cdar al))
             (idx (alist-ref key key-indices =?))
             (al (cdr al)))
        (if idx
            (begin
              (vector-set! vec idx val)
              (alist-values/single-traverse al (alist-delete-first key key-indices =?) vec =?))
            (alist-values/single-traverse al key-indices vec =?)))))

;;; Returns the values corresponding to keys of an alist, with optional default
;;; values.
;;;
;;; (alist-values =? ALIST LVAR ...)
;;;
;;; LVAR ::= KEY
;;;        | (KEY)
;;;        | (KEY DEFAULT)
;;;
;;; (alist-values eq? '((foo . "foo") (bar . "bar") (zaz . "zaz")) 'foo 'zaz)
;;; ;=> (values "foo" "zaz")
;;;
;;; (alist-values eq? '((foo . "foo") (bar . "bar") (zaz . "zaz")) 'foo 'nop)
;;; ;=> (values "foo" #f)
;;;
;;; (alist-values eq? '((foo . "foo") (bar . "bar") (zaz . "zaz")) 'foo ('nop 42))
;;; ;=> (values "foo" 42)
;;;
;;; This macro expands in three phases:
;;;  1. add-defaults => Traverses the keys list and adds the default value for
;;;     the keys that don't have one.
;;;  2. reverse => The previous phase reverses the order of the keys in the
;;;     process, so it must be reversed again to give the correct final result.
;;;  3. result => Finally, after the inputs have been "normalized", we can
;;;     expand to the final expression.
(define-syntax alist-values
  (syntax-rules (quote quasiquote)
    ; All inputs have a default value and equality test now, so they have to be
    ; reversed.
    ((alist-values "add-defaults" tmp-list =? alist ())
     (alist-values "reverse" tmp-list =? alist ()))

    ; Expressions like 'abc and `abc are matched as (quote abc) and (quasiquote abc)
    ((alist-values "add-defaults" tmp-list =? alist ((quote key) keys ...))
     (alist-values "add-defaults" tmp-list =? alist (((quote key)) keys ...)))
    ((alist-values "add-defaults" tmp-list =? alist ((quasiquote key) keys ...))
     (alist-values "add-defaults" tmp-list =? alist (((quasiquote key)) keys ...)))

    ; This key has an explicit default value and equality test, just move it to
    ; the tmp-list.
    ((alist-values "add-defaults" (tmp-list ...)               =? alist ((key default) keys ...))
     (alist-values "add-defaults" ((key default) tmp-list ...) =? alist (keys ...)))

    ; Add a default value of #f if there's no explicit one.
    ((alist-values "add-defaults" tmp-list =? alist ((key) keys ...))
     (alist-values "add-defaults" tmp-list =? alist ((key #f) keys ...)))

    ; This clause is still necessary because alist-values may be used by
    ; itself without alist-let.
    ((alist-values "add-defaults" tmp-list =? alist (key keys ...))
     (alist-values "add-defaults" tmp-list =? alist ((key #f) keys ...)))


    ((alist-values "reverse" () =? alist keys)
     (alist-values "result" =? alist keys))

    ((alist-values "reverse" (key tmp-list ...) =? alist (keys ...))
     (alist-values "reverse" (tmp-list ...)     =? alist (key keys ...)))


    ((alist-values "result" =? alist ((key default) ...))
     (let* ((keys (list key ...))
            (key-indices (map cons keys (iota (length keys))))
            (vec (list->vector (list default ...))))
       (alist-values/single-traverse alist key-indices vec =?)))


    ((alist-values =? alist key ...)
     (alist-values "add-defaults" () =? alist (key ...)))))


;;;
;;; (alist-let =? ALIST (LVAR ...)
;;;   BODY ...)
;;;
;;; LVAR ::= (VAR KEY)
;;;        | (VAR KEY DEFAULT)
;;;
(define-syntax alist-let
  (syntax-rules ()
    ((alist-let =? alist ((var key opts ...) ...)
       body ...)
     (receive (var ...) (alist-values =? alist (key opts ...) ...)
       body ...))))
