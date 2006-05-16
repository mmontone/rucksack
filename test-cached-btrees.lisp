
;; NOTE: This is old code that doesn't work anymore.  It needs to be
;; adapted to the current version of Rucksack.

(defpackage :test-btree
  (:use :cl :btree :rucksack))

(in-package :test-btree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *cache-dir* #P"/tmp/btrees/")

(defun test ()
  (with-cache (cache *cache-dir*)
    (let ((btree (make-instance 'btree)))
      (btree-insert btree 0 'zero)
      (btree-insert btree 15 'fifteen)
      (btree-insert btree 10 'ten)
      ;;
      (values (cached-object-id btree)
              (list (btree-search btree 0)
                    (btree-search btree 10)
                    (btree-search btree 15)
                    (btree-search btree 42 :errorp nil))))))


(defun test-insert (&key (n 20000) (node-size 100) (commit-interval nil))
  (with-cache (cache *cache-dir*)
    (let ((btree (make-instance 'btree :value= 'string-equal
                                :max-node-size node-size)))
      (loop for i from 1 to n
            for key = (random n) do
            (when (zerop (mod i 1000))
              (format t "~D " i))
            (btree-insert btree key (format nil "~R" key))
            (when (and commit-interval (zerop (mod i commit-interval)))
              (cache-commit cache)))
      ;; Return the id of the cached btree, so it can be used later.
      (cached-object-id btree))))

(defun test-dummy-insert (&key (n 20000))
  (loop for i from 1 to n
        for key = (random n)
        when (zerop (mod i 1000)) do (format t "~D " i)
        collect (cons key (format nil "~R" key)))
  'ok)


(defun test-map (btree-id &key (display t))
  ;; Uses the id returned by test-insert.
  (with-cache (cache *cache-dir*)
    (let ((btree (cache-get-object btree-id cache)))
      (map-btree btree
                 (lambda (key value)
                   (when display
                     (format t "~&~D is '~A' in English." key value)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Timing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

TEST-BTREE 9 >  (time (test-insert :n 100000 :node-size 10))
Timing the evaluation of (TEST-INSERT :N 100000 :NODE-SIZE 10)
1000 2000 3000 4000 5000 6000 7000 8000 9000 10000 11000 12000 13000 14000 15000 16000 17000 18000 19000 20000 21000 22000 23000 24000 25000 26000 27000 28000 29000 30000 31000 32000 33000 34000 35000 36000 37000 38000 39000 40000 41000 42000 43000 44000 45000 46000 47000 48000 49000 50000 51000 52000 53000 54000 55000 56000 57000 58000 59000 60000 61000 62000 63000 64000 65000 66000 67000 68000 69000 70000 71000 72000 73000 74000 75000 76000 77000 78000 79000 80000 81000 82000 83000 84000 85000 86000 87000 88000 89000 90000 91000 92000 93000 94000 95000 96000 97000 98000 99000 100000 
user time    =     26.093
system time  =      6.953
Elapsed time =   0:00:33
Allocation   = 113278680 bytes standard / 99056749 bytes conses
0 Page faults
0

TEST-BTREE 10 > (time (test-map 0 :display nil))
Timing the evaluation of (TEST-MAP 0 :DISPLAY NIL)

user time    =      7.328
system time  =      1.937
Elapsed time =   0:00:09
Allocation   = 26434448 bytes standard / 12417614 bytes conses
0 Page faults
NIL

TEST-BTREE 11 >  (time (test-insert :n 100000 :node-size 10 :commit-interval 1))
Timing the evaluation of (TEST-INSERT :N 100000 :NODE-SIZE 10 :COMMIT-INTERVAL 1)
1000 2000 3000 4000 5000 6000 7000 8000 9000 10000 11000 12000 13000 14000 15000 16000 17000 18000 19000 20000 21000 22000 23000 24000 25000 26000 27000 28000 29000 30000 31000 32000 33000 34000 35000 36000 37000 38000 39000 40000 41000 42000 43000 44000 45000 46000 47000 48000 49000 50000 51000 52000 53000 54000 55000 56000 57000 58000 59000 60000 61000 62000 63000 64000 65000 66000 67000 68000 69000 70000 71000 72000 73000 74000 75000 76000 77000 78000 79000 80000 81000 82000 83000 84000 85000 86000 87000 88000 89000 90000 91000 92000 93000 94000 95000 96000 97000 98000 99000 100000 
user time    =    197.968
system time  =    147.656
Elapsed time =   0:05:58
Allocation   = 1238953896 bytes standard / 219146763 bytes conses
0 Page faults
0

TEST-BTREE 12 > (time (test-map 0 :display nil))
Timing the evaluation of (TEST-MAP 0 :DISPLAY NIL)

user time    =      7.843
system time  =      1.812
Elapsed time =   0:00:10
Allocation   = 34591528 bytes standard / 12403732 bytes conses
0 Page faults
NIL

|#
