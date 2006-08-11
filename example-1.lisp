;; $Id: example-1.lisp,v 1.1 2006-08-11 12:52:53 alemmens Exp $

(in-package :test-rucksack)

;; NOTE: This example doesn't run at the moment, because indexing doesn't
;; work correctly yet.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indexing, class redefinitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *example-1* #p"/tmp/rucksack/example-1/")

(defparameter *hackers* '("David" "Jim" "Peter" "Thomas"
                          "Arthur" "Jans" "Klaus" "James" "Martin"))

(with-rucksack (rucksack *example-1* :if-exists :supersede)
  (with-transaction ()

    ;; For classes that may change during program development, you should
    ;; wrap all class definitions in a WITH-RUCKSACK to make sure that
    ;; the corresponding schema definitions and indexes are updated correctly.
    ;; (This is only necessary if you already have a rucksack that contains
    ;; instances of the class that's being redefined, of course.)
    
    ;; Define a class hacker
    (defclass hacker ()
      ((id :initform (gensym "HACKER-")
           :reader hacker-id
           :index :symbol-index
           :unique t)
       (name :initform (elt *hackers* (random (length *hackers*)))
             :accessor name
             :index :case-insensitive-string-index)
       (age :initform (random 100) :accessor age))
      (:metaclass persistent-class))))
  

(defmethod print-object ((hacker hacker) stream)
  (print-unreadable-object (hacker stream :type t)
    (format stream "~S called ~S of age ~D"
            (hacker-id hacker)
            (name hacker)
            (age hacker))))

(defun example-1 ()
  (with-rucksack (rucksack *example-1*)
    ;; Fill the rucksack with some hackers.
    (with-transaction ()
      (loop repeat 1000
            do (make-instance 'hacker))
      #+nil
      (rucksack-map-slot rucksack 'hacker 'name
                         (lambda (hacker)
                           (print-object hacker *standard-output*)
                           (terpri))))))

(defun show-hackers ()
  (with-rucksack (rucksack *example-1*)
    (rucksack-map-class rucksack 'hacker
                        (lambda (hacker)
                          (print-object hacker *standard-output*)
                          (terpri)))))

