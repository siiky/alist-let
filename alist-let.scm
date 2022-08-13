(import chicken.base)

;;; Returns the values corresponding to keys of an alist, with optional default
;;; values.
;;;
;;; (alist-values '((foo . "foo") (bar . "bar") (zaz . "zaz")) 'foo 'zaz)
;;; ;=> (values "foo" "zaz")
;;;
;;; (alist-values '((foo . "foo") (bar . "bar") (zaz . "zaz")) 'foo 'nop)
;;; ;=> (values "foo" #f)
;;;
;;; (alist-values '((foo . "foo") (bar . "bar") (zaz . "zaz")) 'foo ('nop 42))
;;; ;=> (values "foo" 42)
;;;
;;; This macro expands in three phases:
;;;  1. add-defaults => Traverses the keys list and adds the default value and
;;;     equality test for the keys that don't have one.
;;;  2. reverse => The previous phase reverses the order of the keys in the
;;;     process, so it must be reversed again to give the correct final result.
;;;  3. result => Finally, after the inputs have been "normalized", we can
;;;     expand to the final expression.
(define-syntax alist-values
  (syntax-rules (quote quasiquote)
    ((alist-values "add-defaults" tmp-list alist ())
     (alist-values "reverse" tmp-list alist ()))

    ((alist-values "add-defaults" tmp-list alist ((quote key) keys ...))
     (alist-values "add-defaults" tmp-list alist (((quote key)) keys ...)))
    ((alist-values "add-defaults" tmp-list alist ((quasiquote key) keys ...))
     (alist-values "add-defaults" tmp-list alist (((quasiquote key)) keys ...)))

    ((alist-values "add-defaults" (tmp-list ...)                      alist ((key default equal?) keys ...))
     (alist-values "add-defaults" ((key default equal?) tmp-list ...) alist (keys ...)))

    ((alist-values "add-defaults" tmp-list alist ((key default) keys ...))
     (alist-values "add-defaults" tmp-list alist ((key default equal?) keys ...)))

    ; Hack to make the `alist-let` implementation simpler, so there's no need
    ; to do the `add-defaults` phase in `alist-let` too.
    ((alist-values "add-defaults" tmp-list alist ((key) keys ...))
     (alist-values "add-defaults" tmp-list alist ((key #f) keys ...)))

    ; This clause is still necessary because alist-values may be used by
    ; itself, without alist-let.
    ((alist-values "add-defaults" tmp-list alist (key keys ...))
     (alist-values "add-defaults" tmp-list alist ((key #f) keys ...)))

    ((alist-values "reverse" () alist keys)
     (alist-values "result" alist keys))

    ((alist-values "reverse" (key tmp-list ...) alist (keys ...))
     (alist-values "reverse" (tmp-list ...)     alist (key keys ...)))

    ((alist-values "result" alist ((key default equal?) ...))
     (let ((%alist alist))
       ; TODO: How to implement this with better performance?
       (values (alist-ref key %alist equal? default) ...)))

    ((alist-values alist key ...)
     (alist-values "add-defaults" () alist (key ...)))))

(define-syntax alist-let
  (syntax-rules ()
    ((alist-let alist ((var key opts ...) ...)
       body ...)
     (receive (var ...) (alist-values alist (key opts ...) ...)
       body ...))))
