;; $Id: example-1.lisp,v 1.2 2006-08-26 12:55:34 alemmens Exp $

(in-package :test-rucksack)

;; NOTE: At the moment, this example works only when this file is compiled
;; exactly once.  After the second compile, slot indexing will fail (because
;; ENSURE-CLASS-SCHEMA isn't complete yet).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indexing, class redefinitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *example-1* #p"/tmp/rucksack/example-1/"))

(defparameter *hackers* '("David" "Jim" "Peter" "Thomas"
                          "Arthur" "Jans" "Klaus" "James" "Martin"))

(defun random-elt (list)
  (elt list (random (length list))))

(eval-when (:compile-toplevel)
  (with-rucksack (*rucksack* *example-1* :if-exists :supersede)
    (with-transaction ()

      ;; For classes that may change during program development, you should
      ;; wrap all class definitions in a WITH-RUCKSACK to make sure that
      ;; the corresponding schema definitions and indexes are updated correctly.
      ;; (This is only necessary if you already have a rucksack that contains
      ;; instances of the class that's being redefined, of course.)
    
      (defclass hacker ()
        ((id :initform (gensym "HACKER-")
             :reader hacker-id
             :index :symbol-index
             :unique t)
         (name :initform (random-elt *hackers*)
               :accessor name
               :index :case-insensitive-string-index)
         (age :initform (random 100) :accessor age
              :index :number-index))
        (:metaclass persistent-class)
        (:index t))

      (defclass lisp-hacker (hacker)
        ()
        (:metaclass persistent-class)
        (:index t)))))


(defmethod print-object ((hacker hacker) stream)
  (print-unreadable-object (hacker stream :type t)
    (format stream "~S called ~S of age ~D"
            (hacker-id hacker)
            (name hacker)
            (age hacker))))

(defun example-1 ()
  (with-rucksack (*rucksack* *example-1*)
    ;; Fill the rucksack with some hackers.
    (with-transaction ()
      (loop repeat 20
            do (make-instance 'hacker))
      (loop repeat 10
            do (make-instance 'lisp-hacker))
      (rucksack-map-class *rucksack* 'hacker #'print))))

(defun show-hackers ()
  (with-rucksack (*rucksack* *example-1*)
    (with-transaction ()
      (print "Hackers indexed by object id.")
      (rucksack-map-class *rucksack* 'hacker #'print)
      (print "Hackers indexed by name.")
      (rucksack-map-slot *rucksack* 'hacker 'name #'print)
      (print "Hackers indexed by hacker-id.")
      (rucksack-map-slot *rucksack* 'hacker 'id #'print)
      (print "Lisp hackers.")
      (rucksack-map-class *rucksack* 'lisp-hacker #'print)
      (print "Non-lisp hackers.")
      (rucksack-map-class *rucksack* 'hacker #'print
                          :include-subclasses nil)
      (print "Hacker object ids.")
      (rucksack-map-class *rucksack* 'hacker #'print
                          :id-only t))))

(defun show-indexes ()
  (with-rucksack (r *example-1*)
    (print (rs::rucksack-list-class-indexes r))
    (print (rs::rucksack-list-slot-indexes r))
    :ok))
