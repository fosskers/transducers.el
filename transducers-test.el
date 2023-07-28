(require 'ert)
(require 'transducers)

(ert-deftest transducers-map ()
  (should (equal '(2 3 4) (t/transduce (t/map #'1+) #'t/cons '(1 2 3)))))
