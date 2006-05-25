;; $Id: test.lisp,v 1.4 2006-05-25 13:01:38 alemmens Exp $

(in-package :test-rucksack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A few quick tests to make sure the basics work.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *test-suite* #p"/tmp/rucksack-test-suite/")

(defmacro p-test (form test)
  `(progn
     (with-rucksack (in *test-suite* :if-exists :supersede)
       (with-transaction ()
         (add-rucksack-root ,form in)))
     (with-rucksack (out *test-suite* :if-exists :overwrite)
       (with-transaction ()
         (let ((all (rucksack-roots out)))
           (assert (= 1 (length all)))
           (let ((it (car all)))
             (assert ,test)))))))

(defmacro test (form)
  `(assert ,form))

(defclass p-thing-1 ()
  ()
  (:metaclass persistent-class))

(defclass p-thing-2 ()
  ((x :initarg :x :reader x-of :persistence t))
  (:metaclass persistent-class))
  
(defun test-basics ()
  ;;
  ;; Serializing/deserializing pathnames.
  ;;

  (let ((store (merge-pathnames *test-suite* "store")))
    (rucksack::save-objects (list store) store)
    (test (equal (list store) (rucksack::load-objects store))))

  (test (not (current-rucksack)))

  ;;
  ;; P-CONS, P-CAR, P-CDR, P-LIST, P-MAKE-ARRAY, P-AREF
  ;;

  (p-test (p-cons 1 2)
          (and (= 1 (p-car it)) (= 2 (p-cdr it))))
  
  (test (not (current-rucksack))) ; WITH-RUCKSACK should not leave one around
  
  (p-test (p-list 1 2 3)
	  (equal '(1 2 3)
		 (list (p-car it) (p-car (p-cdr it)) (p-car (p-cdr (p-cdr it))))))
  
  (p-test (p-make-array 2 :initial-contents '(a b))
	  (equal '(a b)
	       (list (p-aref it 0) (p-aref it 1))))
  
  ;;
  ;; Persistent-objects
  ;;
  
  (p-test (make-instance 'p-thing-1)
	  (eq (find-class 'p-thing-1) (class-of it)))
  
  (p-test (make-instance 'p-thing-2 :x "-x-")
	  (equal (x-of it) "-x-"))

  ;;
  ;; Btree basics
  ;;

  (p-test (let ((btree (make-instance 'btree)))
            (btree-insert btree 0 'zero)
            (btree-insert btree 15 'fifteen)
            (btree-insert btree 10 'ten)
            btree)
          (equal (list (btree-search it 0)
                       (btree-search it 10)
                       (btree-search it 15)
                       (btree-search it 42 :errorp nil))
                 '(zero ten fifteen nil)))

  (test (not (current-rucksack)))
  (write-line "basic tests ok"))

(eval-when (:load-toplevel)
  (test-basics))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test basic create, load and update functionality with many objects, so
;;; the incremental garbage collector needs to do some work too.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *names* '("David" "Jim" "Peter" "Thomas"
                        "Arthur" "Jans" "Klaus" "James" "Martin"))

(defclass person ()
  ((name :initform (elt *names* (random (length *names*)))
         :accessor name)
   (age :initform (random 100) :accessor age))
  (:metaclass persistent-class))

(defmethod print-object ((person person) stream)
  (print-unreadable-object (person stream :type t)
    (format stream "called ~S of age ~D"
            (name person)
            (age person))))

(defun test-create (&key (nr-objects 100000))
  "Test creating a rucksack with many persons."
  (with-rucksack (rucksack *test-suite* :if-exists :supersede)
    (with-transaction ()
      (loop for i below nr-objects
            do (let ((person (make-instance 'person)))
                 (when (zerop (mod i 1000))
                   (format t "~D " i))
                 (add-rucksack-root person rucksack))))))

  
(defun test-update (&key (new-age 27))
  "Test updating all persons by changing their age."
  (with-rucksack (rucksack *test-suite*)
    (with-transaction ()
      (map-rucksack-roots (lambda (person)
                            (setf (age person) new-age))
                          rucksack))))

(defun test-load ()
  "Test loading all persons by computing their average age."
  (with-rucksack (rucksack *test-suite*)
    (with-transaction ()
      (let ((nr-persons 0)
            (total-age 0))
        (map-rucksack-roots (lambda (person)
                              (incf nr-persons)
                              (incf total-age (age person)))
                            rucksack)
        ;; Return the average age as a float.
        ;; (An average age of 1200/75 doesn't seem right.)
        (coerce (/ total-age nr-persons) 'float)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Btrees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Test btrees as just another persistent data structure.
;;

(defun test-btree-insert (&key (n 20000) (node-size 100))
  ;; Create a rucksack with btree that maps random integers to the
  ;; equivalent strings in Roman notation.
  (with-rucksack (rucksack *test-suite* :if-exists :supersede)
    (with-transaction ()
      (let ((btree (make-instance 'btree :value= 'string-equal
                                  :max-node-size node-size)))
        (loop for i from 1 to n
              for key = (random n) do
              (when (zerop (mod i 1000))
                (format t "~D " i))
              (btree-insert btree key (format nil "~R" key)))
        (add-rucksack-root btree rucksack)))))

(defun test-btree-dummy-insert (&key (n 20000))
  ;; This function can be used for timing: subtract the time taken
  ;; by this function from the time taken by TEST-BTREE-INSERT to
  ;; get an estimate of the time needed to manipulate the btrees.
  (loop for i from 1 to n
        for key = (random n)
        when (zerop (mod i 1000)) do (format t "~D " i)
        collect (cons key (format nil "~R" key)))
  t)


(defun test-btree-map (&key (display t))
  ;; Print out the contents of the btree.
  (with-rucksack (rucksack *test-suite*)
    (with-transaction ()
      (let ((btree (first (rucksack-roots rucksack))))
        (map-btree btree
                   (lambda (key value)
                     (when display
                       (format t "~&~D -> ~A~%" key value))))))))
