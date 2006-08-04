;; $Id: test.lisp,v 1.5 2006-08-04 22:04:43 alemmens Exp $

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

(defun shuffle (array)
  (loop with n = (array-dimension array 0)
        repeat n
        for i = (random n)
        for j = (random n)
        when (/= i j)
        do (rotatef (aref array i) (aref array j))))

(defun check-size (btree expected)
  (format t "~&Counting~%")
  (let ((count 0))
    (map-btree btree
               (lambda (key value)
                 (declare (ignore key value))
                 (incf count)))
    (unless (= count expected)
      (error "Wrong btree size - expected ~A, got ~A."
             expected count))))

(defun check-order (btree)
  (format t "~&Checking order~%")
  (rs::check-btree btree))

(defun check-contents (btree)
  (format t "~&Checking contents~%")
  (map-btree btree
             (lambda (key value)
               (unless (string= value (format nil "~R" key))
                 (error "Value mismatch: Expected ~S, got ~S."
                        (format nil "~R" key) value)))))

(defmacro with-transaction* ((&rest args) &body body)
  `(with-transaction ,args
     (prog1 (progn ,@body)
       (format t "~&Committing..."))))

(defun test-btree (&key (n 20000) (node-size 100) (delete (floor n 10)) check-contents)
  ;; Create a rucksack with a btree of size N that maps random
  ;; integers to the equivalent strings as a cardinal English number.
  ;; Use node size NODE-SIZE for the btree.
  ;; If DELETE is not NIL, delete and reinsert that number of elements
  ;; as well.
  (let ((array (make-array n :initial-contents (loop for i below n collect i))))
    (shuffle array)
    (with-rucksack (rucksack *test-suite* :if-exists :supersede)
      (with-transaction* ()
        (format t "~&Inserting~%")
        (let ((btree (make-instance 'btree :value= 'string-equal
                                    :max-node-size node-size)))
          (loop for key across array
                for i from 1
                when (zerop (mod i 1000))
                do (format t "~D " i)
                do (btree-insert btree key (format nil "~R" key)))
          (add-rucksack-root btree rucksack))))
    (with-rucksack (rucksack *test-suite*)
      (with-transaction ()
        (let ((btree (first (rucksack-roots rucksack))))
          (check-order btree)
          (check-size btree n)
          (when check-contents
            (check-contents btree))))
      (when delete
        (shuffle array)
        (setq array (subseq array 0 delete))
        (shuffle array)
        (with-transaction* ()
          (format t "~&Deleting~%")
          (let ((btree (first (rucksack-roots rucksack))))
            (dotimes (i delete)
              (when (zerop (mod (1+ i) 1000))
                (format t "~D " (1+ i)))
              (btree-delete btree (aref array i)))
            (check-order btree)
            (check-contents btree)))
        (with-transaction* ()
          (let ((btree (first (rucksack-roots rucksack))))
            (check-order btree)
            (check-size btree (- n delete))
            (when check-contents
              (check-contents btree))
            (format t "~&Reinserting~%")
            (shuffle array)
            (dotimes (i delete)
              (when (zerop (mod (1+ i) 1000))
                (format t "~D " (1+ i)))
              (let ((key (aref array i)))
                (btree-insert btree key (format nil "~R" key))))))
        (with-transaction ()
          (let ((btree (first (rucksack-roots rucksack))))
            (check-order btree)
            (check-size btree n)
            (when check-contents
              (check-contents btree)))))))
  :ok)



(defun test-btree-map (&key (display t))
  ;; Print out the contents of the btree.
  (with-rucksack (rucksack *test-suite*)
    (with-transaction ()
      (let ((btree (first (rucksack-roots rucksack))))
        (map-btree btree
                   (lambda (key value)
                     (when display
                       (format t "~&~D -> ~A~%" key value))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Garbage collector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-gc (n)
  (with-rucksack (rucksack *test-suite* :if-exists :supersede)
    (with-transaction ()
      ;; after this, INNER can be reached directly from the root
      (let* ((inner (p-cons "Waldorf" "Statler"))
             (root (p-cons 42 inner)))
        (add-rucksack-root root rucksack)))
    (with-transaction ()
      (let* ((root (first (rucksack-roots rucksack)))
             (inner (p-cdr root))
             (array (p-make-array n)))
        ;; after this, INNER can't be reached from the root anymore
        (setf (p-cdr root) 43)
        ;; now let the GC do some work
        (dotimes (i n)
          (let ((string (format nil "~R" i)))
            (setf (p-aref array i) (p-cons string string))))
        ;; hook INNER back to the root again before we finish the
        ;; transaction
        (setf (p-car root) array
              (p-cdr root) (p-cons 'bar (p-cons 'foo inner)))))
    (with-transaction ()
      (let* ((root (first (rucksack-roots rucksack)))
             (inner (p-cdr (p-cdr (p-cdr root)))))
        ;; we expect the list ("Waldorf" "Statler") here
        (list (p-car inner) (p-cdr inner))))))
