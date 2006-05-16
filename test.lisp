(in-package :test-rucksack)

(defparameter *test-suite* #p"/tmp/rucksack-test-suite/")

;;;; A few quick tests to make sure basics work
(macrolet ((p-test (form test)
	     `(let (item)
		(with-rucksack (in *test-suite* :if-exists :supersede)
		  (with-transaction ()
		    (add-rucksack-root (setq item ,form) in)))
		(with-rucksack (out *test-suite* :if-exists :overwrite)
		  (with-transaction ()
		    (let ((all (rucksack-roots out)))
		      (assert (= 1 (length all)))
		      (let ((it (car all)))
			(assert ,test)))))))
	   (test (form)
	     `(assert ,form)))
  
  (let ((store (merge-pathnames *test-suite* "store")))
    (rucksack::save-objects (list store) store)
    (test (equal (list store) (rucksack::load-objects store))))

  (test (not (current-rucksack)))

  (p-test (p-cons 1 2)
	(and (= 1 (p-car it)) (= 2 (p-cdr it))))
  
  (test (not (current-rucksack))) ; WITH-RUCKSACK should not leave one around
  
  (p-test (p-list 1 2 3)
	  (equal '(1 2 3)
		 (list (p-car it) (p-car (p-cdr it)) (p-car (p-cdr (p-cdr it))))))
  
  (p-test (p-make-array 2 :initial-contents '(a b))
	  (equal '(a b)
	       (list (p-aref it 0) (p-aref it 1))))
  
  (defclass p-thing-1 () ()
    (:metaclass persistent-class))
  
  (p-test (make-instance 'p-thing-1)
	  (eq (find-class 'p-thing-1) (class-of it)))
  
  (defclass p-thing-2 ()
    ((x :initarg :x :reader x-of :persistence t))
    (:metaclass persistent-class))
  
  (p-test (make-instance 'p-thing-2 :x "-x-")
	  (equal (x-of it) "-x-"))'

  (test (not (current-rucksack)))
  (write-line "basic tests ok"))


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


(defparameter *persons-directory* #P"/tmp/persons/")

(defun test-create (&key (nr-objects 100000) (directory *persons-directory*))
  "Test creating a rucksack with many persons."
  (with-rucksack (rucksack directory)
    (with-transaction ()
      (loop for i below nr-objects
            do (let ((person (make-instance 'person)))
                 (when (zerop (mod i 1000))
                   (format t "~D " i))
                 (add-rucksack-root person rucksack))))))

  
(defun test-update (&key (new-age 27) (directory *persons-directory*))
  "Test updating all persons by changing their age."
  (with-rucksack (rucksack directory)
    (map-rucksack-roots (lambda (person)
                          (setf (age person) new-age))
                        rucksack)))

(defun test-load (&key (directory *persons-directory*))
  "Test loading all persons by computing their average age."
  (with-rucksack (rucksack directory)
    (let ((nr-persons 0)
          (total-age 0))
      (map-rucksack-roots (lambda (person)
                            (incf nr-persons)
                            (incf total-age (age person)))
                          rucksack)
      ;; Return the average age as a float.
      ;; (An average age of 1200/75 doesn't seem right.)
      (coerce (/ total-age nr-persons) 'float))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; timings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
TEST-RS 25 > (time (test-create :nr-objects 100000))

|#
