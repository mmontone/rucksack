
;; NOTE: This is old code that doesn't work anymore.  It needs to be
;; adapted to the current version of Rucksack.

(defpackage :cache-test 
  (:use :cl :rucksack))

(in-package :cache-test)

(defparameter *names* '("Arthur" "Edi" "Pascal" "Kenny" "Joe"
                        "Kent" "Steve" "Duane" "Marc" "Bill"))

(defclass person (cached-clos-object)
  ((name :initform (elt *names* (random (length *names*)))
         :accessor name)
   (age :initform (random 100) :accessor age))
  #+lispworks
  (:optimize-slot-access nil))

(defmethod print-object ((person person) stream)
  (print-unreadable-object (person stream :type t)
    (format stream "called ~S of age ~D"
            (name person)
            (age person))))


(defparameter *persons-cache* #P"/tmp/persons/")

(defun test-create (&key (nr-objects 100000) (directory *persons-cache*))
  (with-cache (cache directory)
    (let ((ids (loop repeat nr-objects
                     collect (let* ((person (make-instance 'person))
                                    (id (rucksack::object-id person)))
                               (when (zerop (mod id 1000))
                                 (format t "~D " id))
                               id))))
      ids)))

(defun test-load (object-ids &key (directory *persons-cache*))
  (with-cache (cache directory)
    (loop for id in object-ids
          collect (cache-get-object id cache))))

  
(defun test-update (object-ids &key (new-age 27) (directory *persons-cache*))
  (with-cache (cache directory)
    (loop for id in object-ids
          do (let ((person (cache-get-object id cache)))
               (setf (age person) new-age)))))


(defun average-age (object-ids &key (directory *persons-cache*))
  (with-cache (cache directory)
    (let ((nr-persons 0)
          (total-age 0))
      (loop for id in object-ids
            do (let ((person (cache-get-object id cache)))
                 (incf nr-persons)
                 (incf total-age (age person))))
      (coerce (/ total-age nr-persons) 'float))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; timings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
CACHE-TEST 25 > (time (progn (setq *ids* (test-create :nr-objects 100000)) 'ok))
Timing the evaluation of (PROGN (SETQ *IDS* (TEST-CREATE :NR-OBJECTS 100000)) (QUOTE OK))
1000 2000 3000 4000 5000 6000 7000 8000 9000 10000 11000 12000 13000 14000 15000 16000 17000 18000 19000 20000 21000 22000 23000 24000 25000 26000 27000 28000 29000 30000 31000 32000 33000 34000 35000 36000 37000 38000 39000 40000 41000 42000 43000 44000 45000 46000 47000 48000 49000 50000 51000 52000 53000 54000 55000 56000 57000 58000 59000 60000 61000 62000 63000 64000 65000 66000 67000 68000 69000 70000 71000 72000 73000 74000 75000 76000 77000 78000 79000 80000 81000 82000 83000 84000 85000 86000 87000 88000 89000 90000 91000 92000 93000 94000 95000 96000 97000 98000 99000 100000 
user time    =      5.140
system time  =      5.062
Elapsed time =   0:00:11
Allocation   = 6631304 bytes standard / 5585195 bytes conses
0 Page faults
OK

CACHE-TEST 26 > (time (progn (test-load  *ids*) 'ok))
Timing the evaluation of (PROGN (TEST-LOAD *IDS*) (QUOTE OK))

user time    =      2.609
system time  =      1.578
Elapsed time =   0:00:04
Allocation   = 8188792 bytes standard / 4408030 bytes conses
0 Page faults
OK
|#
