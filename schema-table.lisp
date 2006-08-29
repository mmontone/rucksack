;; $Id: schema-table.lisp,v 1.4 2006-08-29 11:41:40 alemmens Exp $

(in-package :rucksack)          

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Schema table
;;;
;;; The schema table keeps track of all classes that have instances that
;;; were saved by the cache.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Schema
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass schema ()
  ((id :initarg :id :reader schema-id
       :documentation "A unique number that identifies a schema.")
   (class-name :initarg :class-name :reader schema-class-name)
   (version :initarg :version :initform 0 :reader schema-version
            :documentation "The combination of class-name and version number
also uniquely identifies a schema.")
   ;; Slot info
   ;; DO: Keep track of all slots: their names, their initforms and their
   ;; persistence related slot options.
   ;; PERSISTENT-SLOT-NAMES is set during FINALIZE-INHERITANCE.
   (persistent-slot-names :initarg :persistent-slot-names
                          :accessor persistent-slot-names
                          :documentation "A list with the names of all
persistent effective slots.")
   ;; Class info
   (class-index :initarg :class-index :reader class-index)))

(defmethod nr-persistent-slots ((schema schema))
  (length (persistent-slot-names schema)))

(defmethod print-object ((schema schema) stream)
  (print-unreadable-object (schema stream :type t :identity t)
    (format stream "~A ~D.~D with ~D slots"
            (schema-class-name schema)
            (schema-id schema)
            (schema-version schema)
            (nr-persistent-slots schema))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass schema-table ()
  ((pathname :initarg :pathname :accessor schema-table-pathname)
   (by-name :initform (make-hash-table)
            :documentation "A mapping from class names to a list (most
recent version first) of schemas."
            :reader schema-table-by-name)
   (by-id :initform (make-hash-table)
          :documentation "A mapping from a schema id to a schema."
          :accessor schema-table-by-id)
   (highest-schema-id :initform 0 :accessor highest-schema-id)
   (dirty-p :initform nil :accessor dirty-p
            :documentation "When dirty, the schema table will be saved
at the next commit.")))

;;
;; Serializing schema table
;;

(defmethod saved-slots ((table schema-table))
  ;; Don't serialize the BY-ID hash table, but rebuild it by hand after the
  ;; other slots are deserialized.  This is necessary because schemas are
  ;; referenced more than once, and the serializer doesn't handle shared
  ;; objects (unless they're 'real' persistent objects).
  '(pathname by-name highest-schema-id))

(defmethod load-slots :after ((table schema-table) serializer)
  ;; Reconstruct the BY-ID hash table.  This method is called by the
  ;; serializer after an object is deserialized.
  (setf (schema-table-by-id table) (make-hash-table))
  (loop for schemas being the hash-value of (schema-table-by-name table)
        do (loop for schema in schemas
                 do (setf (gethash (schema-id schema)
                                   (schema-table-by-id table))
                          schema)))
  ;;
  (setf (dirty-p table) nil)
  table)

;;
;; Finding schemas
;;

(defmethod fresh-schema-id ((table schema-table))
  (prog1 (highest-schema-id table)
    (incf (highest-schema-id table))))

(defmethod find-schema-for-id ((table schema-table) id &key (errorp t))
  (or (gethash id (schema-table-by-id table))
      (and errorp
           (error "Can't find schema with id ~D in ~S." id table))))

(defmethod find-schema-for-class ((table schema-table) class)
  ;; Returns the most recent schema for a class
  ;; (or NIL if there is no schema for the class).
  (first (gethash (class-name class) (schema-table-by-name table))))

(defmethod schema-obsolete-p ((table schema-table) schema)
  (let ((most-recent-schema (find-schema-for-class table
                                                   (schema-class-name schema))))
    (not (= (schema-version most-recent-schema)
            (schema-version schema)))))

(defmethod find-or-create-schema-for-object ((table schema-table) object)
  ;; NOTE: This assumes that the class hasn't changed without the
  ;; schema table knowing about it.  We probably must assume that,
  ;; otherwise we'd have a very expensive check whenever we want to
  ;; save an object.
  (let ((class (class-of object)))
    (or (find-schema-for-class table class)
        ;; There is no schema yet.  Create it.
        (let ((persistent-slots (compute-persistent-slot-names class object)))
          (create-schema table class 0 persistent-slots)))))


(defmethod create-schema ((table schema-table) class version
                          &optional (persistent-slots '()))
  (let ((schema (make-instance 'schema
                               :id (fresh-schema-id table)
                               :class-name (class-name class)
                               :version version
                               :persistent-slot-names persistent-slots
                               :class-index (compute-class-index class))))
    (add-schema table schema)
    schema))

                                    
(defmethod compute-persistent-slot-names ((class persistent-class) object)
  (declare (ignore object))
  (mapcar #'slot-definition-name (class-persistent-slots class)))

(defgeneric compute-class-index (class)
  (:method ((class persistent-class))
   (class-index class))
  (:method ((class t))
   nil))
                   
(defmethod add-schema ((table schema-table) (schema schema))
  (setf (gethash (schema-id schema) (schema-table-by-id table))
        schema)
  (push schema
        (gethash (schema-class-name schema) (schema-table-by-name table) '()))
  (setf (dirty-p table) t))


(defmethod save-schema-table ((table schema-table))
  ;; Clear dirty flag first, because it's saved (and loaded) too.
  (setf (dirty-p table) nil) 
  (save-objects (list table) (schema-table-pathname table)))

(defmethod save-schema-table-if-necessary ((table schema-table))
  (when (dirty-p table)
    (save-schema-table table)))

(defun open-schema-table (pathname &key if-exists if-does-not-exist)
  ;; Load existing schemas from the file.
  (if (probe-file pathname)
      (ecase if-exists
        (:error (error "Schema table file ~S already exists." pathname))
        (:supersede
         ;; Create an empty schema table, save it and return it.
         (let ((table (make-instance 'schema-table :pathname pathname)))
           (save-schema-table table)
           table))
        (:overwrite
         ;; Normal case
         (let ((table (first (load-objects pathname))))
           (when (not (equal pathname (schema-table-pathname table)))
             ;; The table was moved; update the pathname info.
             (setf (schema-table-pathname table) pathname)
             (save-schema-table table))
           table)))
    (ecase if-does-not-exist
      (:error (error "Schema table file ~S does not exist." pathname))
      (:create
         ;; Create an empty schema table, save it and return it.
         (let ((table (make-instance 'schema-table :pathname pathname)))
           (save-schema-table table)
           table)))))


(defun close-schema-table (table &key (commit t))
  (when (and commit (dirty-p table))
    (save-schema-table table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Schema updates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod maybe-update-schema ((table schema-table) class old-slots)
  ;; Rucksack analyzes the new class definition; if it's different from the
  ;; previous version, a new schema is added to the schema table.  From that
  ;; moment, when an instance of the redefined class is created it will be
  ;; saved with the new schema id.
  ;; This is called by the (RE-)INITIALIZE-INSTANCE method for
  ;; PERSISTENT-CLASS.
  (let ((old-schema (find-schema-for-class table class)))
    (if (null old-schema)
        ;; There is no schema yet: create the first one.
        (create-schema table class 0)
      ;; There is a schema: create a new one if necessary.
      (multiple-value-bind (added-slots discarded-slots changed-slots)
          (compare-slots old-slots (class-direct-slots class))
        (when (or added-slots discarded-slots changed-slots
                  (not (equal (class-index class) (class-index old-schema))))
          ;; Add a new schema for this class.
          (create-schema table class (1+ (schema-version old-schema))))))))
