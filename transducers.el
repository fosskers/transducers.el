;;; transducers.el --- Ergonomic, efficient data processing -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Colin Woodbury
;;
;; Author: Colin Woodbury <colin@fosskers.ca>
;; Maintainer: Colin Woodbury <colin@fosskers.ca>
;; Created: July 26, 2023
;; Modified: July 26, 2023
;; Version: 0.0.1
;; Keywords: lisp
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

(cl-defgeneric t/transduce (xform f source &rest sources)
  "The entry-point for processing some data source via transductions.

Given a composition of transducer functions (the XFORM), a
reducer function F, a concrete data SOURCE, and any number
of additional SOURCES, perform a full, strict transduction.")

(cl-defmethod t/transduce (xform f (source list) &rest sources)
  "Transduce over lists.

Given a composition of transducer functions (the XFORM), a
reducer function F, a concrete list SOURCE, and any number of
additional lists SOURCES, perform a full, strict transduction."
  (transducers--list-transduce xform f source sources))

(cl-defmethod t/transduce (xform f (source array) &rest sources)
  "Transduce over arrays.

Given a composition of transducer functions (the XFORM), a
reducer function F, a concrete array SOURCE, and any number of
additional array SOURCES, perform a full, strict transduction."
  (transducers--array-transduce xform f source sources))

(defun transducers--list-transduce (xform f coll &optional colls)
  "Transduce over lists.

Given a composition of transducer functions (the XFORM), a
reducer function F, a concrete list COLL, and any number of
additional lists COLLS, perform a full, strict transduction."
  (let* ((init   (funcall f))
         (xf     (funcall xform f))
         (result (transducers--list-reduce xf init coll colls)))
    (funcall xf result)))

(defun transducers--list-reduce (f identity coll &optional colls)
  "Reduce over lists.

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

(defun transducers--array-transduce (xform f coll &optional colls)
  "Transduce over arrays.

Given a composition of transducer functions (the XFORM), a
reducer function F, a concrete array COLL, and any number of
additional array COLLS, perform a full, strict transduction."
  (let* ((init   (funcall f))
         (xf     (funcall xform f))
         (result (transducers--array-reduce xf init coll colls)))
    (funcall xf result)))

(defun transducers--array-reduce (f identity arr &optional arrs)
  "Reduce over arrays.

F is the transducer/reducer composition, IDENTITY the result of
applying the reducer without arguments (thus achieving an
\"element\" or \"zero\" value), ARR is our guaranteed source
array, and ARRS are any additional source arrays."
  (let ((shortest (apply #'min (length arr) (mapcar #'length arrs))))
    (cl-labels ((recurse (acc i)
                  (if (= i shortest)
                      acc
                    (let ((acc (apply f acc (aref arr i) (mapcar (lambda (a) (aref a i)) arrs))))
                      (if (reduced-p acc)
                          (reduced-val acc)
                        (recurse acc (1+ i)))))))
      (recurse identity 0))))

;; --- Transducers --- ;;

(defun t/pass (reducer)
  "Transducer: Just pass along each value of the transduction.

Same in intent with applying `t/map' to `identity', but this
should be slightly more efficient. It is at least shorter to
type.

This function is expected to be passed \"bare\" to `t/transduce',
so there is no need for the caller to manually pass a REDUCER."
  (lambda (result &rest inputs)
    (if inputs (apply reducer result inputs)
      (funcall reducer result))))

;; (t/transduce #'t/pass #'+ '(1 2 3))

(defun t/map (f)
  "Transducer: Apply a function F to all elements of the transduction."
  (lambda (reducer)
    (lambda (result &rest inputs)
      (if inputs (funcall reducer result (apply f inputs))
        (funcall reducer result)))))

;; (t/transduce (t/map (lambda (n) (+ 1 n))) #'+ '(1 2 3))
;; (t/transduce (t/map #'*) #'+ '(1 2 3) '(4 5 6 7))

;; --- Reducers --- ;;

(defun t/cons (&rest vargs)
  "Reducer: Collect all results as a list.

Regardings VARGS: as a \"reducer\", this function expects zero to
two arguments."
  (pcase vargs
    (`(,acc ,input) (cons input acc))
    (`(,acc) (reverse acc))
    (`() '())))

;; (t/transduce (t/map #'1+) #'t/cons '(1 2 3))

(defun t/string (&rest vargs)
  "Reducer: Collect all results as a string.

Regardings VARGS: as a \"reducer\", this function expects zero to
two arguments."
  (pcase vargs
    (`(,acc ,input) (cons input acc))
    (`(,acc) (cl-concatenate 'string (reverse acc)))
    (`() '())))

(defun t/vector (&rest vargs)
  "Reducer: Collect all results as a vector.

Regardings VARGS: as a \"reducer\", this function expects zero to
two arguments."
  (pcase vargs
    (`(,acc ,input) (cons input acc))
    (`(,acc) (cl-concatenate 'vector (reverse acc)))
    (`() '())))

(defun t/count (&rest vargs)
  "Reducer: Count the number of elements that made it through the transduction.

Regardings VARGS: as a \"reducer\", this function expects zero to
two arguments."
  (pcase vargs
    (`(,acc ,_) (1+ acc))
    (`(,acc) acc)
    (`() 0)))

(defun t/average (fallback)
  "Reducer: Calculate the average value of all numeric elements in a transduction.

A FALLBACK must be provided in case no elements made it through
the transduction (thus protecting from division-by-zero)."
  (let ((items 0))
    (lambda (&rest vargs)
      (pcase vargs
        (`(,acc ,input) (+ acc input))
        (`(,acc) (if (= 0 items) fallback
                   (/ acc items)))
        (`() 0)))))

(defun t/any (pred)
  "Reducer: Yield non-nil if any element in the transduction satisfies PRED.

Short-circuits the transduction as soon as the condition is met."
  (lambda (&rest vargs)
    (pcase vargs
      (`(,_ ,input) (if (funcall pred input)
                        ;; NOTE We manually return `t' here because there is no
                        ;; guarantee that `input' iteslf was not `nil' and still
                        ;; passed the `if' when given to `pred'!
                        (make-reduced :val t)
                      nil))
      (`(,acc) acc)
      (_ nil))))

;; (t/transduce #'t/pass (t/any #'cl-evenp) '(1 3 5 7 9 2))

(defun t/all (pred)
  "Reducer: Yield non-nil if all elements of the transduction satisfy PRED.

Short-circuits with nil if any element fails the test."
  (lambda (&rest vargs)
    (pcase vargs
      (`(,acc ,input) (if (and acc (funcall pred input))
                          t
                        (make-reduced :val nil)))
      (`(,acc) acc)
      (_ t))))

;; (t/transduce #'t/pass (t/all #'cl-oddp) '(1 3 5 7 9))

(provide 'transducers)
;;; transducers.el ends here
