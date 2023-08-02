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

(defun transducers--preserving-reduced (reducer)
  "Given a REDUCER, wraps a reduced value twice.
This is because reducing functions (like
`transducers--list-reduce') unwraps them. `t/concatenate' is a
good example: it re-uses its reducer on its input using
list-reduce. If that reduction finishes early and returns a
reduced value, `transducers--list-reduce' would unreduce' that
value and try to continue the transducing process."
  (lambda (a b)
    (let ((result (funcall reducer a b)))
      (if (reduced-p result)
          (make-reduced :val result)
        result))))

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

(defun t/concatenate (reducer)
  "Transducer: Concatenate all the sublists in the transduction.

This function is expected to be passed \"bare\" to `t/transduce',
so there is no need for the caller to manually pass a REDUCER."
  (let ((preserving-reducer (transducers--preserving-reduced reducer)))
    (lambda (result &optional inputs)
      (if inputs (transducers--list-reduce preserving-reducer result inputs)
        (funcall reducer result)))))

;; (t/transduce #'t/concatenate #'t/cons '((1 2 3) (4 5 6) (7 8 9)))

(defun t/flatten (reducer)
  "Transducer: Entirely flatten all lists in the transduction.

This function is expected to be passed \"bare\" to `t/transduce',
so there is no need for the caller to manually pass a REDUCER."
  (lambda (result &rest inputs)
    ;; FIXME Tue Aug  1 21:07:16 2023
    ;;
    ;; Only considers the first input element.
    (if inputs (let ((input (car inputs)))
                 ;; FIXME Tue Aug  1 21:09:53 2023
                 ;;
                 ;; Why is this only considering lists?
                 (if (listp input)
                     (transducers--list-reduce (transducers--preserving-reduced (t/flatten reducer)) result input)
                   (funcall reducer result input)))
      (funcall reducer result))))

;; (t/transduce #'t/flatten #'t/cons '((1 2 3) 0 (4 (5) 6) 0 (7 8 9) 0))

(defun t/segment (n)
  "Transducer: Partition the input into lists of N items.

 If the input stops, flush any accumulated state, which may be
shorter than N."
  (unless (> n 0)
    (error "The arguments to segment must be a positive integer"))
  (lambda (reducer)
    (let ((i 0)
          (collect '()))
      (lambda (result &rest inputs)
        (cond (inputs
               ;; FIXME Wed Aug  2 11:44:27 2023
               ;;
               ;; Only the first input is considered.
               (setf collect (cons (car inputs) collect))
               (setf i (1+ i))
               (if (< i n)
                   result
                 (let ((next-input (reverse collect)))
                   (setf i 0)
                   (setf collect '())
                   (funcall reducer result next-input))))
              (t (let ((result (if (zerop i)
                                   result
                                 (funcall reducer result (reverse collect)))))
                   (setf i 0)
                   (if (reduced-p result)
                       (funcall reducer (reduced-val result))
                     (funcall reducer result)))))))))

;; (t/transduce (t/segment 3) #'t/cons '(1 2 3 4 5))

(defun t/group-by (f)
  "Transducer: Group the input stream into sublists via some function F.

The cutoff criterion is whether the return value of F changes
between two consecutive elements of the transduction."
  (lambda (reducer)
    (let ((prev 'nothing)
          (collect '()))
      (lambda (result &rest inputs)
        (if inputs (let* ((input (car inputs)) ;; FIXME Only considers the first input.
                          (fout (funcall f input)))
                     (if (or (equal fout prev) (eq prev 'nothing))
                         (progn (setf prev fout)
                                (setf collect (cons input collect))
                                result)
                       (let ((next-input (reverse collect)))
                         (setf prev fout)
                         (setf collect (list input))
                         (funcall reducer result next-input))))
          (let ((result (if (null collect)
                            result
                          (funcall reducer result (reverse collect)))))
            (setf collect '())
            (if (reduced-p result)
                (funcall reducer (reduced-val result))
              (funcall reducer result))))))))

;; (t/transduce (t/group-by #'cl-evenp) #'t/cons '(2 4 6 7 9 1 2 4 6 3))

(defun t/intersperse (elem)
  "Transducer: Insert an ELEM between each value of the transduction."
  (lambda (reducer)
    (let ((send-elem? nil))
      (lambda (result &rest inputs)
        (if inputs (if send-elem?
                       (let ((result (funcall reducer result elem)))
                         (if (reduced-p result)
                             result
                           (funcall reducer result (car inputs))))
                     (progn (setf send-elem? t)
                            (funcall reducer result (car inputs))))
          (funcall reducer result))))))

;; (t/transduce (t/intersperse 0) #'t/cons '(1 2 3))

(defun t/enumerate (reducer)
  "Transducer: Index every value passed through the transduction into a cons pair.

Starts at 0.

This function is expected to be passed \"bare\" to `t/transduce',
so there is no need for the caller to manually pass a REDUCER."
  (let ((n 0))
    (lambda (result &rest inputs)
      (if inputs (let ((input (cons n (car inputs))))
                   (setf n (1+ n))
                   (funcall reducer result input))
        (funcall reducer result)))))

;; (t/transduce #'t/enumerate #'t/cons '("a" "b" "c"))

(defun t/log (logger)
  "Transducer: Call some LOGGER function for each step of the transduction.

The LOGGER must accept the running results and the current
\(potentially multiple) elements as input. The original results of
the transduction are passed through as-is."
  (lambda (reducer)
    (lambda (result &rest inputs)
      (if inputs (progn (apply logger result inputs)
                        (apply reducer result inputs))
        (funcall reducer result)))))

;; (t/transduce (t/log (lambda (_ n) (message "Got: %d" n))) #'t/cons '(1 2 3 4 5))

(defun t/window (n)
  "Transducer: Yield N-length windows of overlapping values.

This is different from `t/segment' which yields non-overlapping
windows. If there were fewer items in the input than N, then this
yields nothing."
  (unless (> n 0)
    (error "The arguments to window must be a positive integer."))
  (lambda (reducer)
    (let ((i 0)
          (q (make-ring n)))
      (lambda (result &rest inputs)
        (cond (inputs
               (ring-insert-at-beginning q (car inputs))
               (setf i (1+ i))
               (if (< i n) result
                 (funcall reducer result (ring-elements q))))
              (t (funcall reducer result)))))))

;; (t/transduce (t/window 3) #'t/cons '(1 2 3 4 5))

(defun t/unique (reducer)
  "Transducer: Only allow values to pass through the transduction once each.

Stateful; this uses a set internally so could get quite heavy if
you're not careful.

This function is expected to be passed \"bare\" to `t/transduce',
so there is no need for the caller to manually pass a REDUCER."
  (let ((seen (make-hash-table :test #'equal)))
    (lambda (result &rest inputs)
      (if inputs (if (gethash (car inputs) seen) ;; FIXME Only considers first input.
                     result
                     (progn (puthash (car inputs) t seen)
                            (funcall reducer result (car inputs))))
          (funcall reducer result)))))

;; (t/transduce #'t/unique #'t/cons '(1 2 1 3 2 1 2 "abc"))

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
