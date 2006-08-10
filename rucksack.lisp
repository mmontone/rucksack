;; $Id: rucksack.lisp,v 1.9 2006-08-10 12:36:17 alemmens Exp $

(in-package :rucksack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rucksacks: API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; open-rucksack [Function]
;; close-rucksack [Function]
;; with-rucksack [Macro]
;; current-rucksack [Function]

;; commit [Function]
;; rollback [Function]

(defgeneric add-rucksack-root (object rucksack)
  (:documentation
 "Adds an object to the root set of a rucksack."))

(defgeneric delete-rucksack-root (object rucksack)
  (:documentation
 "Delete an object from the root set of a rucksack."))

(defgeneric map-rucksack-roots (function rucksack)
  (:documentation
 "Applies a function to all objects in the root set of a rucksack."))

(defgeneric rucksack-roots (rucksack)
  (:documentation
 "Returns a list with all objects in the root set of a rucksack.  You
shouldn't modify this list."))

(defgeneric rucksack-cache (rucksack)
  (:documentation "Returns the cache for a rucksack."))

(defgeneric rucksack-directory (rucksack)
  (:documentation
 "Returns a pathname for the directory that contains all files of a
rucksack."))

(defgeneric rucksack-commit (rucksack)
  (:documentation
 "Ensures that all in-memory data is saved to disk."))

(defgeneric rucksack-rollback (rucksack)
  ;; DO: What does rollback mean exactly here?
  (:documentation "...."))

;;
;;  Class and slot indexing
;;

;; add-class-index (class-designator &key errorp)  [Function]
;; add-slot-index (class-designator slot index-spec &key errorp) [Function]
;; remove-class-index (class-designator &key errorp) [Function]
;; remove-slot-index (class-designator slot &key errorp) [Function]
;; map-class-indexes (function) [Function]
;; map-slot-indexes (function &key class include-subclasses) [Function]


(defgeneric rucksack-update-class-index (rucksack class)
  (:documentation 
   "Compares the current class index for CLASS to the class index
that's specified in the :INDEX class options of CLASS.  An obsolete
class index (i.e. a class index that's specified anymore in the class
option) is removed, new class indexes are added."))

(defgeneric rucksack-update-slot-indexes (rucksack class)
  (:documentation 
   "Compares the current slot indexes for CLASS to the slot indexes
that are specified in the slot options for the direct slots of CLASS.
Obsolete slot indexes (i.e. slot indexes that are not specified
anymore in the slot options) are removed, new slot indexes are
added."))

(defgeneric rucksack-add-class-index (rucksack class-designator &key errorp))

(defgeneric rucksack-remove-class-index (rucksack class-designator
                                                  &key errorp))

(defgeneric rucksack-class-index (rucksack class-designator &key errorp)
  (:documentation "Returns the class index for a class designator."))

(defgeneric rucksack-map-class-indexes (rucksack function)
  (:documentation
   "FUNCTION must take two arguments: a class name and a class index.
It is called for all class indexes in the specified rucksack."))

(defgeneric rucksack-make-class-index (rucksack class &key index-spec)
  (:documentation
   "Creates a new class index and returns that index.  INDEX-SPEC
specifies the kind of index that must be created (if not supplied, the
rucksack's default class index spec will be used."))


(defgeneric rucksack-add-slot-index (rucksack class-designator slot index-spec
                                              &key errorp)
  (:documentation
  "Creates a new slot index for the slot designated by
CLASS-DESIGNATOR and SLOT.  The type of index is specified by
INDEX-SPEC.  Returns the new index.  Signals an error if ERRORP is T
and there is already an index for the designated slot."))

(defgeneric rucksack-remove-slot-index (rucksack class-designator slot
                                                 &key errorp))



(defgeneric rucksack-slot-index (rucksack class-designator slot &key errorp)
  (:documentation
 "Returns the slot index for the slot specified by CLASS-DESIGNATOR
and SLOT."))


(defgeneric rucksack-map-slot-indexes (rucksack function
                                       &key class include-subclasses)
  (:documentation
   "FUNCTION must take three arguments: a class name, a slot name and
a slot index.  It is called for all slot indexes in the specified
rucksack.
  CLASS defaults to T, meaning all classes.
  INCLUDE-SUBCLASSES defaults to T."))

(defgeneric rucksack-maybe-index-changed-slot (rucksack 
                                               class object slot
                                               old-value new-value
                                               old-boundp new-boundp)
  (:documentation
 "This function is called after a slot has changed.  OLD-VALUE is the
slot's value before the change, NEW-VALUE is the current value.
OLD-BOUNDP is true iff the slot was bound before the change,
NEW-BOUNDP is true iff the slot is currently bound."))

(defgeneric rucksack-maybe-index-new-object (rucksack class-designator object)
  (:documentation
 "Adds the object id of OBJECT to the class index for the class
designated by CLASS-DESIGNATOR.  If there is no such class index, it
does nothing."))

(defgeneric rucksack-map-class (rucksack class function
                                &key id-only include-subclasses)
  (:documentation
 "  FUNCTION is a unary function that gets called for all instances of
the specified class.  Unindexed classes (i.e. classes for which the
:indexed class option is nil) will be skipped.
  If ID-ONLY is T (default is NIL), the function will be called with
object ids instead of 'real' objects.  This can be handy if you want to
do more filtering before actually loading objects from disk.
  INCLUDE-SUBCLASSES defaults to T."))

(defgeneric rucksack-map-slot (rucksack class slot function
                              &key equal min max include-min include-max order
                              id-only include-subclasses)
  (:documentation
 "  FUNCTION is a unary function that gets called for all instances of
the specified class that have a slot value matching the EQUAL, MIN,
MAX INCLUDE-MIN and INCLUDE-MAX arguments.  ORDER can be either
:ASCENDING (default) or :DESCENDING; currently, the specified order
will be respected for instances of one class but not across subclasses.
  If ID-ONLY is T (default is NIL), the function will be called with
object ids instead of 'real' objects.  This can be handy if you want to
do more filtering before actually loading objects from disk.
  INCLUDE-SUBCLASSES defaults to T."))


#+later
(defgeneric rucksack-map-objects (rucksack class-designator function
                                           slots filter order)
  (:documentation
 " Applies FUNCTION to all instances of the class designated by
CLASS-DESIGNATOR for which the criteria specified by SLOTS and
CRITERIA hold.
  SLOTS is a list of slot names.  FILTER is a filter expression that can
refer to the slot names.
  Example of a filter expression: (and (= age 20) (string= city \"Hamburg\"))
"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Locks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-lock (&key (name "lock"))
  #+allegro
  (mp:make-process-lock :name name)
  #+lispworks
  (mp:make-lock :name name)
  #+sbcl
  (sb-thread:make-mutex :name name)
  #+openmcl
  (ccl:make-lock name)
  #-(or allegro lispworks sbcl openmcl)
  (not-implemented 'make-lock))


(defmacro with-lock ((lock) &body body)
  #+allegro
  `(mp:with-process-lock (,lock) ,@body)
  #+lispworks
  `(mp:with-lock (,lock) ,@body)
  #+sbcl
  `(sb-thread:with-mutex (,lock) ,@body)
  #+openmcl
  `(ccl:with-lock-grabbed (,lock) ,@body)
  #-(or allegro lispworks sbcl openmcl)
  (not-implemented 'with-lock))

(defun process-lock (lock)
  #+lispworks
  (mp:process-lock lock)
  #-lispworks
  (not-implemented 'process-lock))

(defun process-unlock (lock)
  #+lispworks
  (mp:process-unlock lock)
  #-lispworks
  (not-implemented 'process-unlock))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rucksacks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass rucksack ()
  ())

(defclass standard-rucksack (rucksack)
  ((cache :reader rucksack-cache)
   (directory :initarg :directory :reader rucksack-directory)
   (roots :initform '()
          :documentation
 "A list with the object ids of all root objects, i.e.  the objects
from which the garbage collector can reach all live objects.")
   (roots-changed-p :initform nil :accessor roots-changed-p)
   ;; Indexes
   (class-index-table :initform (make-hash-table)
                      :documentation
 "A mapping from class names to indexes.  Each index contains the ids
of all instances from a class."
                      :reader class-index-table)
   (slot-index-tables :initform (make-hash-table)
                      :reader slot-index-tables
                      :documentation
 "A mapping from class names to slot index tables, where each slot
index table is a mapping from slot names to slot indexes.  Each slot
index maps slot values to object ids.")))

(defmethod print-object ((rucksack rucksack) stream)
  (print-unreadable-object (rucksack stream :type t :identity t)
    (format stream "in ~S with ~D root~:P"
            (rucksack-directory rucksack)
            (length (slot-value rucksack 'roots)))))

(defmethod rucksack-roots-pathname ((rucksack standard-rucksack))
  (merge-pathnames "roots" (rucksack-directory rucksack)))


(defmethod initialize-instance :after ((rucksack standard-rucksack)
                                       &key
                                       (cache-class 'standard-cache)
                                       (cache-args '())
                                       &allow-other-keys)
  ;; Open cache.
  (setf (slot-value rucksack 'cache)
        (apply #'open-cache (rucksack-directory rucksack)
               :class cache-class
               :rucksack rucksack
               cache-args))
  (load-roots rucksack))


(defun load-roots (rucksack)
  ;; Read roots (i.e. object ids) from the roots file (if there is one).
  (let ((roots-file (rucksack-roots-pathname rucksack)))
    (when (probe-file roots-file)
      (setf (slot-value rucksack 'roots)
            (load-objects roots-file)))))

(defun save-roots (rucksack)
  (save-objects (slot-value rucksack 'roots)
                (rucksack-roots-pathname rucksack))
  (setf (roots-changed-p rucksack) nil))

(defun save-roots-if-necessary (rucksack)
  (when (roots-changed-p rucksack)
    (save-roots rucksack)))
  
(defmethod add-rucksack-root (object (rucksack standard-rucksack))
  (add-rucksack-root-id (object-id object) rucksack))

(defun add-rucksack-root-id (object-id rucksack)
  (push object-id (slot-value rucksack 'roots))
  (setf (roots-changed-p rucksack) t))

(defmethod delete-rucksack-root (object (rucksack standard-rucksack))
  (with-slots (roots)
      rucksack
    (setf roots (delete (object-id object) roots)
          (roots-changed-p rucksack) t)))

(defmethod map-rucksack-roots (function (rucksack standard-rucksack))
  (loop for root-id in (slot-value rucksack 'roots)
        do (funcall function
                    (cache-get-object root-id (rucksack-cache rucksack)))))


(defmethod rucksack-roots ((rucksack standard-rucksack))
  (let ((result '()))
    (map-rucksack-roots (lambda (root) (push root result))
                        rucksack)
    ;; We don't need to nreverse the list, because the order isn't specified.
    result))

;;
;; Opening
;;

(defparameter *rucksack-opening-lock*
  (make-lock :name "Rucksack opening lock"))
 
(defun open-rucksack (directory-designator 
                      &rest args
                      &key 
                      (class 'serial-transaction-rucksack)
                      (if-exists :overwrite) (if-does-not-exist :create)
                      (cache-class 'standard-cache) (cache-args '())
                      &allow-other-keys)
  "Opens the rucksack in the directory designated by DIRECTORY-DESIGNATOR.
  :IF-DOES-NOT-EXIST can be either :CREATE (creates a new rucksack if the
it does not exist; this is the default) or :ERROR (signals an error if
the rucksack does not exist).
  :IF-EXISTS can be either :OVERWRITE (loads the rucksack if it exists;
this is the default), :SUPERSEDE (deletes the existing rucksack and creates
a new empty rucksack) or :ERROR (signals an error if the rucksack exists)."
  (declare (ignorable cache-class cache-args))
  (check-type directory-designator (or string pathname))
  (check-type if-exists (member :overwrite :supersede :error))
  (check-type if-does-not-exist (member :create :error))
  (let ((directory (if (stringp directory-designator)  
                      (pathname directory-designator)
                      directory-designator)))
    (with-lock (*rucksack-opening-lock*)
      (setq *rucksack*
            (if (probe-file (merge-pathnames "roots" directory))
                ;; Rucksack already exists.
                (ecase if-exists
                  (:error
                   (error "Can't create rucksack in ~S: the directory
already seems to contain a rucksack."
                          directory))
                  (:supersede
                   ;; Remove all rucksack files from the directory.
 		   ;; DO: Only delete the files that Rucksack actually
                   ;; uses.
 		   (mapc #'delete-file
 			 (directory (make-pathname :name :wild
 						   :type :wild
 						   :version :wild
 						   :defaults directory)))
 		   (apply #'make-instance class :directory directory args))
                  (:overwrite
                   ;; This is the normal case.
                   (apply #'make-instance class :directory directory args)))
              ;; Rucksack doesn't seem to exist.
              (ecase if-does-not-exist
                (:error
                 (error "Can't open rucksack in ~S: the rucksack roots
file is missing."
                        directory))
                (:create
                 (ensure-directories-exist directory)
                 (apply #'make-instance class :directory directory args))))))))


(defun close-rucksack (rucksack &key (commit t))
  (when commit
    (rucksack-commit rucksack))
  ;; If :COMMIT is true, the cache and transaction handler are already
  ;; committed by the rucksack-commit, so we close them without committing.
  (close-cache (rucksack-cache rucksack) :commit nil))

;;
;; Commit
;;

(defun commit (&key (rucksack (current-rucksack)))
  (rucksack-commit rucksack))

(defmethod rucksack-commit ((rucksack standard-rucksack))
  (cache-commit (rucksack-cache rucksack))
  (when (roots-changed-p rucksack)
    (save-roots rucksack)))

;;
;; Rollback
;;

(defun rollback (&key (rucksack (current-rucksack)))
  (rucksack-rollback rucksack))

(defmethod rucksack-rollback ((rucksack standard-rucksack))
  ;; Rollback the cache.
  (cache-rollback (rucksack-cache rucksack))
  ;; Rollback the roots by loading them back from file.
  (load-roots rucksack)
  (setf (roots-changed-p rucksack) nil))

(defmacro with-rucksack ((rucksack directory &rest args) &body body)
   `(let* ((*rucksack* *rucksack*)
           (,rucksack (open-rucksack ,directory ,@args)))
      (unwind-protect (progn ,@body)
        (close-rucksack ,rucksack))))


(defun test-garbage-collector (rucksack)
  (collect-garbage (heap (rucksack-cache rucksack))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indexing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod rucksack-update-class-index ((rucksack standard-rucksack)
                                        (class persistent-class))
  (let ((old-index (rucksack-class-index rucksack class :errorp nil))
        (needs-index-p (class-index class)))
    (cond ((and old-index (not needs-index-p))
           (rucksack-remove-class-index rucksack class :errorp t))
          ((and (not old-index) needs-index-p)
           ;; Create a class index now.
           ;; NOTE: If there are existing instances of this class,
           ;; they're *not* automatically indexed at this point.
           ;; (In fact, the only way to do this would be to iterate
           ;; over *all* objects in the rucksack, which would be rather
           ;; expensive.  Then again, it's exactly what the garbage
           ;; collector does anyway, so it may be an option to have the
           ;; garbage collector index them automatically.  But I'm not
           ;; sure if that's a good idea.)
           (rucksack-add-class-index rucksack class :errorp t))
          (t
           ;; We don't need to change anything
           :no-change))))

(defmethod rucksack-update-slot-indexes ((rucksack standard-rucksack)
                                         (class persistent-class))
  (dolist (slot (class-direct-slots class))
    (let ((index-needed (and (slot-persistence slot) (slot-index slot)))
          (current-index (rucksack-slot-index rucksack class slot)))
      (cond ((index-spec-equal index-needed current-index)
             ;; We keep the same index: no change needed.
             :no-change)
            ((and current-index (null index-needed))
             ;; The index is not wanted anymore: remove it.
             (rucksack-remove-slot-index rucksack class slot :errorp t))
            ((and (null current-index) index-needed)
             ;; We didn't have an index but we need one now: add one.
             (rucksack-add-slot-index rucksack class slot index-needed
                                      :errorp t))
            ((and current-index index-needed)
             ;; We have an index but need a different one now.  This requires
             ;; some care because we need to re-index all objects from the old
             ;; index.
             (let ((new-index (rucksack-add-slot-index rucksack class slot
                                                       index-needed
                                                       :errorp nil)))
               ;; Re-index all objects for the new index.
               (map-index current-index
                          (lambda (slot-value object-id)
                            (index-insert new-index slot-value object-id)))
               ;; We don't need to remove the old index explicitly, because
               ;; RUCKSACK-ADD-SLOT-INDEX already did that for us.
               ))))))


;;
;; Some simple dispatchers.
;;

;; Q: Are these really necessary?

(defun add-class-index (class-designator &key errorp)
  (rucksack-add-class-index (current-rucksack) class-designator
                            :errorp errorp))

(defun add-slot-index (class-designator slot index-spec &key (errorp nil))
  (rucksack-add-slot-index (current-rucksack) class-designator slot index-spec
                           :errorp errorp))

(defun remove-class-index (class-designator &key (errorp nil))
  (rucksack-remove-class-index (current-rucksack) class-designator
                               :errorp errorp))

(defun remove-slot-index (class-designator slot &key (errorp nil))
  (rucksack-remove-slot-index (current-rucksack) class-designator slot
                              :errorp errorp))

(defun map-class-indexes (function)
  (rucksack-map-class-indexes (current-rucksack) function))

(defun map-slot-indexes (function &key (class t) (include-subclasses t))
  (rucksack-map-slot-indexes (current-rucksack) function
                             :class class
                             :include-subclasses include-subclasses))

;;
;; Class indexes
;;

(defmethod rucksack-add-class-index ((rucksack standard-rucksack) class
                                     &key (errorp nil))
  (unless (symbolp class)
    (setq class (class-name class)))
  (when (and errorp (gethash class (class-index-table rucksack)))
    (simple-rucksack-error "Class index for ~S already exists in ~A."
                           class
                           rucksack))
  (setf (gethash class (class-index-table rucksack))
        (rucksack-make-class-index rucksack class)))

(defmethod rucksack-make-class-index 
           ((rucksack standard-rucksack) class
            &key
            (index-spec '(btree :key< < :key= = :value= eql :unique-keys-p t)))
  ;; A class index maps object ids to objects.
  (declare (ignore class))
  (make-index index-spec))

(defmethod rucksack-remove-class-index ((rucksack standard-rucksack) class
                                        &key (errorp nil))
  (unless (symbolp class)
    (setq class (class-name class)))
  (when (and errorp
             (not (gethash class (class-index-table rucksack))))
    (simple-rucksack-error "Class index for ~S doesn't exist in ~A."
                           class
                           rucksack))
  (remhash class (class-index-table rucksack)))


(defmethod rucksack-map-class-indexes (rucksack function)
  (maphash function (class-index-table rucksack)))

(defmethod rucksack-class-index ((rucksack standard-rucksack) class
                                 &key (errorp nil))
  (unless (symbolp class)
    (setq class (class-name class)))
  (or (gethash class (class-index-table rucksack))
      (and errorp
           (simple-rucksack-error "Can't find class index for ~S in ~A."
                                  class
                                  rucksack))))

(defmethod rucksack-maybe-index-new-object ((rucksack standard-rucksack)
                                            class object)
  (let ((index (rucksack-class-index rucksack class :errorp nil)))
    (when index
      (index-insert index (object-id object) (object-id object)
                    :if-exists :error))))


(defmethod rucksack-map-class ((rucksack standard-rucksack) class function
                               &key (id-only nil) (include-subclasses t))
  (let ((visited-p (make-hash-table))
        (cache (rucksack-cache rucksack)))
    (labels ((map-instances (class)
               (let ((index (rucksack-class-index rucksack class :errorp nil)))
                 (when index
                   (map-index index
                              (lambda (id ignore)
                                (declare (ignore ignore))
                                (funcall function
                                         (if id-only
                                             id
                                             (cache-get-object id cache)))))
                   (setf (gethash class visited-p) t))
                 (when include-subclasses
                   (loop for class in (class-direct-subclasses
                                       (if (symbolp class)
                                           (find-class class)
                                         class))
                         unless (gethash class visited-p)
                         do (map-instances class))))))
      (map-instances class))))

;;
;; Slot indexing
;;

(defmethod rucksack-add-slot-index ((rucksack standard-rucksack)
                                    class slot index-spec
                                    &key (errorp nil))
  (unless (symbolp class)
    (setq class (class-name class)))
  (unless (symbolp slot)
    (setq slot (slot-definition-name slot)))
  ;; Find the slot index table for CLASS, create a slot index and add that
  ;; index to the table.
  (let* ((slot-index-tables (slot-index-tables rucksack))
         (slot-index-table (or (gethash class slot-index-tables)
                               (let ((table (make-hash-table)))
                                 (setf (gethash class slot-index-tables) table)
                                 table)))
         (new-slot-index (make-index index-spec)))
    ;; Add a new slot index table if necessary.
    (when (and errorp (gethash slot slot-index-table))
      (simple-rucksack-error "Slot index for slot ~S of class ~S
already exists in ~A."
                             slot
                             class
                             rucksack))
    (setf (gethash slot slot-index-table) new-slot-index)))

(defmethod rucksack-remove-slot-index (rucksack class slot &key (errorp nil))
  (unless (symbolp class)
    (setq class (class-name class)))
  (unless (symbolp slot)
    (setq slot (slot-definition-name slot)))
  (flet ((oops ()
           (simple-rucksack-error "Attempt to remove non-existing slot
index for slot ~S of class ~S in ~A."
                                  slot
                                  class
                                  rucksack)))
    (let ((slot-index-table (gethash class (slot-index-tables rucksack))))
      (if slot-index-table
          (if errorp
              (let ((index (gethash slot slot-index-table)))
                (if index
                    (remhash slot slot-index-table)
                  (oops)))
            (remhash slot slot-index-table))
        (and errorp (oops))))))


(defmethod rucksack-map-slot-indexes ((rucksack standard-rucksack) function
                                      &key (class t) (include-subclasses t))
  (if (eql class t)
      (maphash (lambda (class slot-index-table)
                 (maphash (lambda (slot slot-index)
                            (funcall function class slot slot-index))
                          slot-index-table))
               (slot-index-tables rucksack))
    (let ((visited-p (make-hash-table)))
      (flet ((map-indexes (class)
               (unless (gethash class visited-p)
                 (let ((slot-index-table (gethash (class-name class)
                                                  (slot-index-tables rucksack))))
                   (when slot-index-table
                     (maphash (lambda (slot slot-index)
                                (funcall function (class-name class)
                                         slot
                                         slot-index))
                              slot-index-table)))
                 (setf (gethash class visited-p) t)
                 (when include-subclasses
                   (mapc #'map-indexes
                         (class-direct-subclasses class))))))
        (map-indexes (if (symbolp class) (find-class class) class))))))


(defmethod rucksack-maybe-index-changed-slot ((rucksack standard-rucksack)
                                              class object slot
                                              old-value new-value
                                              old-boundp new-boundp)
  (let ((index (rucksack-slot-index rucksack class slot)))
    (when index
      (when old-boundp
        (index-delete index old-value object :if-does-not-exist :ignore))
      (when new-boundp
        (index-insert index new-value object)))))


(defmethod rucksack-slot-index ((rucksack standard-rucksack) class slot
                                &key (errorp nil))
  (unless (symbolp class)
    (setq class (class-name class)))
  (unless (symbolp slot)
    (setq slot (slot-definition-name slot)))
  (let ((slot-index-tables (slot-index-tables rucksack)))
    (flet ((find-index (class)
             (let ((slot-index-table (gethash class slot-index-tables)))
 	       (and slot-index-table
                    (gethash slot slot-index-table)))))
      (or (find-index class)
          (loop for superclass in (class-precedence-list (find-class class))
                thereis (find-index (class-name superclass)))
          (and errorp
               (simple-rucksack-error "Can't find slot index for slot
~S of class ~S in ~A."
                                      slot
                                      class
                                      rucksack))))))


(defmethod rucksack-map-slot ((rucksack standard-rucksack) class slot function
                              &key equal min max include-min include-max
                              (order :ascending)
                              (id-only nil) (include-subclasses t))
  (let ((cache (rucksack-cache rucksack))
        (visited-p (make-hash-table)))
    (labels ((map-slot (class)
               (let ((index (rucksack-slot-index rucksack class slot
                                                 :errorp nil)))
                 (when index
                   ;; The index maps slot values to object ids.
                   (map-index index
                              (lambda (slot-value object-id)
                                (declare (ignore slot-value))
                                (if id-only
                                    (funcall function object-id)
                                  (funcall function
                                           (cache-get-object object-id cache))))
                              :equal equal
                              :min min
                              :max max
                              :include-min include-min
                              :include-max include-max
                              :order order)
                   (setf (gethash class visited-p) t))
                 (when include-subclasses
                   (loop for class in (class-direct-subclasses
                                       (if (symbolp class)
                                           (find-class class)
                                         class))
                         unless (gethash class visited-p)
                         do (map-slot class))))))
      (map-slot (if (symbolp class) (find-class class) class)))))

