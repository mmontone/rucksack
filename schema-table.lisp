(in-package :rucksack)          

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Schema table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The schema table keeps track of all classes that have instances that
;;; were saved by the cache.

(defclass schema ()
  ((id :initarg :id :reader schema-id)
   (class-name :initarg :class-name :reader schema-class-name)
   (version :initarg :version :initform 0 :reader schema-version)
   ;; DO: Keep track of all  slots: their names, their initforms and their
   ;; persistence related slot options (persistence and index).
   ;; Also keep track of persistence related class options here?
   (persistent-slot-names :initarg :persistent-slot-names
                          :reader persistent-slot-names)))

(defmethod nr-persistent-slots ((schema schema))
  (length (persistent-slot-names schema)))

;;
;;
;;

(defclass schema-table ()
  ((pathname :initarg :pathname :accessor schema-table-pathname)
   (by-name :initform (make-hash-table)
            :documentation "A mapping from class names to a list (most
recent version first) of schemas."
            :reader schema-table-by-name)
   (by-id :initform (make-hash-table)
          :documentation "A mapping from a schema id to a schema."
          :reader schema-table-by-id)
   (highest-schema-id :initform 0 :accessor highest-schema-id)
   (dirty-p :initform nil :accessor dirty-p
            :documentation "When dirty, the schema table will be saved
at the next commit.")))

(defmethod find-schema-for-id ((table schema-table) id &key (errorp t))
  (or (gethash id (schema-table-by-id table))
      (and errorp
           (error "Can't find schema with id ~D in ~S." id table))))

(defmethod find-or-create-schema-for-object ((table schema-table) object)
  ;; NOTE: This assumes that the class hasn't changed without the
  ;; schema table knowing about it.  We probably must assume that,
  ;; otherwise we'd have a very expensive check whenever we want to
  ;; save an object.
  (let ((class-name (class-name (class-of object))))
    (or (first (gethash class-name (schema-table-by-name table)))
        ;; There is no schema yet.  Create it.
        (let ((new-schema (create-schema-using-class table
                                                     (class-of object)
                                                     object)))
          (add-schema table new-schema)
          new-schema))))

(defmethod create-schema-using-class ((table schema-table) class object)
  (let ((persistent-slots (compute-persistent-slot-names class object)))
    (make-instance 'schema
                   :class-name (class-name class)
                   :id (highest-schema-id table)
                   :version 0
                   :persistent-slot-names persistent-slots)))

(defmethod compute-persistent-slot-names ((class persistent-class) object)
  (declare (ignore object))
  (mapcar #'slot-definition-name (class-persistent-slots class)))


                   
(defmethod add-schema ((table schema-table) (schema schema))
  (setf (gethash (schema-id schema) (schema-table-by-id table))
        schema)
  (push schema
        (gethash (schema-class-name schema) (schema-table-by-name table) '()))
  (incf (highest-schema-id table))
  (setf (dirty-p table) t))

(defmethod save-schema-table ((table schema-table))
  ;; Clear dirty flag first, because it's saved (and loaded) too.
  (setf (dirty-p table) nil) 
  (save-objects (list table) (schema-table-pathname table)))


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

