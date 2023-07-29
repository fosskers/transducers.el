(require 'ert)
(require 'transducers)

(ert-deftest transducers-collecting ()
  (should (equal '() (t/transduce #'t/pass #'t/cons '())))
  (should (equal [] (t/transduce #'t/pass #'t/vector [])))
  (should (equal "hello" (t/transduce #'t/pass #'t/string "hello"))))

(ert-deftest transducers-counting ()
  (should (= 0 (t/transduce #'t/pass #'t/count '())))
  (should (= 3 (t/transduce #'t/pass #'t/count '(1 2 3))))
  (should (= 0 (t/transduce #'t/pass #'t/count [])))
  (should (= 3 (t/transduce #'t/pass #'t/count [1 2 3]))))

(ert-deftest transducers-map ()
  (should (equal '() (t/transduce (t/map #'1+) #'t/cons '())))
  (should (equal '(2 3 4) (t/transduce (t/map #'1+) #'t/cons '(1 2 3))))
  (should (equal [2 3 4] (t/transduce (t/map #'1+) #'t/vector '(1 2 3))))
  (should (string-equal "HELLO" (t/transduce (t/map #'upcase) #'t/string "hello"))))

(ert-deftest transducers-zip ()
  (should (equal '(5 7 9) (t/transduce (t/map #'+) #'t/cons '(1 2 3) '(4 5 6 7))))
  (should (equal '(6 8 10) (t/transduce (t/map #'+) #'t/cons '(1 2 3 4) '(5 6 7)))))
