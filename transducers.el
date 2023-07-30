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

(defun transducers--ensure-reduced (x)
  "Ensure that X is reduced."
  (if (reduced-p x)
      x
    (make-reduced :val x)))

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

(defun t/filter (pred)
  "Transducer: Only keep elements from the transduction that satisfy PRED."
  (lambda (reducer)
    (lambda (result &rest inputs)
      (if inputs
          (if (apply pred inputs)
              (apply reducer result inputs)
            result)
        (funcall reducer result)))))

(defun t/filter-map (f)
  "Transducer: Filter all non-nil results of the application of F."
  (lambda (reducer)
    (lambda (result &rest inputs)
      (if inputs (let ((x (apply f inputs)))
                   (if x (funcall reducer result x)
                     result))
        (funcall reducer result)))))

;; (t/transduce (t/filter-map #'car) #'t/cons '(() (2 3) () (5 6) () (8 9)))

(defun t/drop (n)
  "Transducer: Drop the first N elements of the transduction."
  (lambda (reducer)
    (let ((new-n (1+ n)))
      (lambda (result &rest inputs)
        (if inputs (progn (setq new-n (1- new-n))
                          (if (> new-n 0)
                              result
                            (apply reducer result inputs)))
          (funcall reducer result))))))

;; (t/transduce (t/drop 3) #'t/cons '(1 2 3 4 5))

(defun t/drop-while (pred)
  "Transducer: Drop elements from the front of the transduction that satisfy PRED."
  (lambda (reducer)
    (let ((drop? t))
      (lambda (result &rest inputs)
        (if inputs (if (and drop? (apply pred inputs))
                       result
                     (progn (setq drop? nil)
                            (apply reducer result inputs)))
          (funcall reducer result))))))

;; (t/transduce (t/drop-while #'cl-evenp) #'t/cons '(2 4 6 7 8 9))

(defun t/take (n)
  "Transducer: Keep only the first N elements of the transduction."
  (lambda (reducer)
    (let ((new-n n))
      (lambda (result &rest inputs)
        (if inputs (let ((result (if (> new-n 0)
                                     (apply reducer result inputs)
                                   result)))
                     (setq new-n (1- new-n))
                     (if (<= new-n 0)
                         (transducers--ensure-reduced result)
                       result))
          (funcall reducer result))))))

;; (t/transduce (t/take 3) #'t/cons '(1 2 3 4 5))
;; (t/transduce (t/take 0) #'t/cons '(1 2 3 4 5))

(defun t/take-while (pred)
  "Transducer: Keep only elements which satisfy PRED.
Stops the transduction as soon as any element fails the test."
  (lambda (reducer)
    (lambda (result &rest inputs)
      (if inputs (if (not (apply pred inputs))
                     (make-reduced :val result)
                   (apply reducer result inputs))
        (funcall reducer result)))))

;; (t/transduce (t/take-while #'cl-evenp) #'t/cons '(2 4 6 8 9 2))

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

(defun t/first (default)
  "Reducer: Yield the first value of the transduction.

If there wasn't one, yields the DEFAULT."
  (lambda (&rest vargs)
    (pcase vargs
      (`(,_ ,input) (make-reduced :val input))
      (`(,acc) acc)
      (_ default))))

;; (t/transduce (t/filter #'cl-oddp) (t/first 0) '(2 4 6 7 10))

(defun t/last (default)
  "Reducer: Yield the last value of the transduction.

If there wasn't one, yields the DEFAULT."
  (lambda (&rest vargs)
    (pcase vargs
      (`(,_ ,input) input)
      (`(,acc) acc)
      (_ default))))

;; (t/transduce #'t/pass (t/last 'none) '(2 4 6 7 10))

(defun t/fold (f seed)
  "Reducer: The fundamental reducer.

`t/fold' creates an ad-hoc reducer based on a given 2-argument
function F. A SEED is also required as the initial accumulator
value, which also becomes the return value in case there were no
input left in the transduction.

Functions like `+' and `*' are automatically valid reducers,
because they yield sane values even when given 0 or 1 arguments.
Other functions like `max' cannot be used as-is as reducers since
they require at least 2 arguments. For functions like this,
`t/fold' is appropriate."
  (lambda (&rest vargs)
    (pcase vargs
      (`(,acc ,input) (funcall f acc input))
      (`(, acc) acc)
      (_ seed))))

(defun t/max (default)
  "Reducer: Yield the maximum value of the transduction.

If there wasn't one, yields the DEFAULT."
  (t/fold #'max default))

(defun t/min (default)
  "Reducer: Yield the minimum value of the transduction.

If there wasn't one, yields the DEFAULT."
  (t/fold #'min default))

(defun t/find (pred)
  "Reducer: Find the first element in the transduction that satisfies a given PRED.

Yields nil if no such element were found."
  (lambda (&rest vargs)
    (pcase vargs
      (`(,_ ,input) (if (funcall pred input)
                        (make-reduced :val input)
                      nil))
      (`(,acc) acc)
      (_ nil))))

(provide 'transducers)
;;; transducers.el ends here
