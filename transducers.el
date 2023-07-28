;;; transducers.el --- Ergonomic, efficient data processing -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Colin Woodbury
;;
;; Author: Colin Woodbury <colin@fosskers.ca>
;; Maintainer: Colin Woodbury <colin@fosskers.ca>
;; Created: July 26, 2023
;; Modified: July 26, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/colin/transducers
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Ergonomic, efficient data processing
;;
;;; Code:

(require 'cl-lib)

(cl-defstruct reduced
  "A wrapper that signals that reduction has completed."
  val)

(defun t/pass (reducer)
  "Just pass along each value of the transduction.

Same in intent with applying `t/map' to `identity', but this
should be slightly more efficient. It is at least shorter to
type.

This function is expected to be passed \"bare\" to `t/transduce',
so there is no need for the caller to manually pass a REDUCER."
  (lambda (result &rest inputs)
    (if inputs (apply reducer result inputs)
      (funcall reducer result))))

;; (transducers--list-transduce #'t/pass #'+ '(1 2 3))

(defun t/map (f)
  "Apply a function F to all elements of the transduction."
  (lambda (reducer)
    (lambda (result &rest inputs)
      (if inputs (funcall reducer result (apply f inputs))
        (funcall reducer result)))))

;; (transducers--list-transduce (t/map (lambda (n) (+ 1 n))) #'+ '(1 2 3))
;; (transducers--list-transduce (t/map #'*) #'+ '(1 2 3) '(4 5 6 7))

(defun transducers--list-transduce (xform f coll &rest colls)
  "Transduce over a list.

Given a composition of transducer functions (the XFORM), a
reducer function F, a concrete list COLL, and any number of
additional lists COLLS, perform a full, strict transduction."
  (let* ((init   (funcall f))
         (xf     (funcall xform f))
         (result (transducers--list-reduce xf init coll colls)))
    (funcall xf result)))

(defun transducers--list-reduce (f identity coll &optional colls)
  "Reduce over a list.

F is the transducer/reducer composition, IDENTITY the result of
applying the reducer without arguments (thus achieving an
\"element\" or \"zero\" value), COLL is our guaranteed source
list, and COLLS are any additional source lists."
  (cl-labels ((recurse (acc items extras)
                (if (or (not items)
                        (cl-some #'not extras))
                    acc
                  (let ((v (apply f acc (car items) (mapcar #'car extras))))
                    (if (reduced-p v)
                        (reduced-val v)
                      (recurse v (cdr items) (mapcar #'cdr extras)))))))
    (recurse identity coll colls)))

(provide 'transducers)
;;; transducers.el ends here

