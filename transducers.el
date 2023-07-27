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
  "Just pass along each value of the transduction. Same in intent with applying
`t/map' to `identity', but this should be slightly more efficient.
It is at least shorter to type."
  (lambda (result &rest inputs)
    (if inputs (apply reducer result inputs)
      (funcall reducer result))))

(defun t/map (f)
  "Apply a function F to all elements of the transduction."
  (lambda (reducer)
    (lambda (result &rest inputs)
      (if inputs (apply reducer result (apply f inputs))
        (funcall reducer result)))))

(defun transducers--list-transduce (xform f coll)
  (let* ((init   (funcall f))
         (xf     (funcall xform f))
         (result (transducers--list-reduce xf init coll)))
    (funcall xf result)))

(defun transducers--list-reduce (f identity lst)
  (cl-labels ((recurse (acc items)
                (if (not items) acc
                  (let ((v (funcall f acc (car items))))
                    (if (reduced-p v)
                        (reduced-val v)
                      (recurse v (cdr items)))))))
    (recurse identity lst)))

(transducers--list-transduce #'t/pass #'+ '(1 2 3))
(transducers--list-transduce (t/map (lambda (n) (+ 1 n))) #'+ '(1 2 3))

(provide 'transducers)
;;; transducers.el ends here
