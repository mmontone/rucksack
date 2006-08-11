;; $Id: mop.lisp,v 1.5 2006-08-11 12:44:21 alemmens Exp $

(in-package :rucksack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MOP Magic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  
;;; Metaclass PERSISTENT-CLASS
;;; 

(defclass persistent-class (standard-class)
  ((persistent-slots :initform '()
                     :accessor class-persistent-slots)
   (index :initarg :index :initform nil :accessor class-index
          :documentation "Can be either NIL (for no class index) or T
(for the standard class index).  Default value is T.")))

(defclass persistent-slot-mixin ()
  ((persistence :initarg :persistence
                :initform t
                :reader slot-persistence
                :documentation "T for persistent slots, NIL for
transient slots.  Default value is T.")
   (index :initarg :index
          :initform nil
          :reader slot-index
          :documentation "An index spec designator for indexed slots,
NIL for non-indexed slots.  Default value is NIL.")
   (unique :initarg :unique
           :initform nil
           :reader slot-unique
           :documentation "Only relevant for indexed slots.  Can be
either NIL (slot values are not unique), T (slot values are unique,
and an error will be signaled for attempts to add a duplicate slot
value) or :NO-ERROR (slot values are unique, but no error will be
signaled for attempts to add a duplicate slot value).  :NO-ERROR
should only be used when speed is critical.
  The default value is NIL.")))

(defclass persistent-direct-slot-definition
    (persistent-slot-mixin standard-direct-slot-definition)
  ())

(defclass persistent-effective-slot-definition
    (persistent-slot-mixin standard-effective-slot-definition)
  ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod validate-superclass ((class standard-class)
                                (superclass persistent-class))
  t)


(defmethod validate-superclass ((class persistent-class)
                                (superclass standard-class))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Processing class and slot options for objects of metaclass
;;; PERSISTENT-CLASS.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+lispworks
(defmethod clos:process-a-slot-option ((class persistent-class)
                                       option
                                       value
                                       already-processed-options
                                       slot)
  (if (member option '(:index :persistence :unique))
      (list* option value already-processed-options)
    (call-next-method)))

#+lispworks
(defmethod clos:process-a-class-option ((class persistent-class)
                                        option-name
                                        value)
  (if (member value '(:index :unique))
      (list option-name value)
    (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initializing the persistent-class metaobjects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The (RE)INITIALIZE-INSTANCE methods below get called whenever a class with
;; metaclass PERSISTENT-CLASS is (re-)defined. When that happens, we: 
;;  - make sure that the class inherits from persistent-object
;;  - create or update schemas.

(defmethod initialize-instance :around ((class persistent-class)
                                        &rest args
                                        &key direct-superclasses
                                        &allow-other-keys)
  ;; Make sure the class inherits from persistent-object.
  (let ((result (apply #'call-next-method
                       class
                       :direct-superclasses (maybe-add-persistent-object-class
                                             class
                                             direct-superclasses)
                       ;; Tell Lispworks that it shouldn't bypass
                       ;; slot-value-using-class.
                       #+lispworks :optimize-slot-access #+lispworks nil 
                       args)))
    (ensure-class-schema class '())
    result))

(defmethod reinitialize-instance :around ((class persistent-class)
                                          &rest args
                                          &key direct-superclasses
                                          &allow-other-keys)
  (let* ((old-slot-defs (class-direct-slots class))
         ;; Create a simple alist with slot name as key and
         ;; a list with slot-index and slot-unique as value.
         (old-slot-indexes (loop for slot-def in old-slot-defs
                                 collect (list (slot-definition-name slot-def)
                                               (slot-index slot-def)
                                               (slot-unique slot-def)))))
    (let ((result (apply #'call-next-method
                         class
                         :direct-superclasses (maybe-add-persistent-object-class
                                               class
                                               direct-superclasses)
                         ;; Tell Lispworks that it shouldn't bypass
                         ;; slot-value-using-class.
                         #+lispworks :optimize-slot-access #+lispworks nil
                         args)))
      (ensure-class-schema class old-slot-indexes)
      result)))


(defun maybe-add-persistent-object-class (class direct-superclasses)
  ;; Add PERSISTENT-OBJECT to the superclass list if necessary.
  (let ((root-class (find-class 'persistent-object nil))
        (persistent-class (find-class 'persistent-class)))
    (if (or (null root-class)
            (eql class root-class)
            (find-if (lambda (direct-superclass)
                       (member persistent-class
                               (compute-class-precedence-list
                                (class-of direct-superclass))))
                     direct-superclasses))
        direct-superclasses
      (cons root-class direct-superclasses))))

(defun ensure-class-schema (class old-slot-indexes)
  ;; Update class and slot indexes.
  (when (some #'slot-persistence (class-direct-slots class))
    ;; NOTE: We get the current-rucksack only if there are some
    ;; persistent slots, because this will also get called during
    ;; compilation of Rucksack (when the class definition of
    ;; PERSISTENT-OBJECT is compiled).  At that stage the CURRENT-RUCKSACK
    ;; function isn't even defined yet, so we shouldn't call it.
    (let ((rucksack (current-rucksack)))
      (when rucksack
        (rucksack-update-class-index rucksack class)
        (rucksack-update-slot-indexes rucksack class old-slot-indexes))))
  ;; DO: Update schema in schema table, when necessary.
  'DO-THIS)


(defmethod finalize-inheritance :after ((class persistent-class))
  ;; Register all persistent slots.
  (setf (class-persistent-slots class)
        (remove-if-not #'slot-persistence (class-slots class))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Computing slot definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod direct-slot-definition-class ((class persistent-class)
                                         &rest initargs)
  (declare (ignore initargs))
  (find-class 'persistent-direct-slot-definition))

(defmethod effective-slot-definition-class ((class persistent-class)
                                            &rest initargs)
  (declare (ignore initargs))
  (find-class 'persistent-effective-slot-definition))



(defmethod compute-effective-slot-definition ((class persistent-class)
                                              slot-name
                                              direct-slot-definitions)
  (let ((effective-slotdef (call-next-method))
        (persistent-slotdefs
         (remove-if-not (lambda (slotdef)
                          (typep slotdef 'persistent-direct-slot-definition))
                        direct-slot-definitions)))

    ;; If any direct slot is persistent, then the effective one is too.
    (setf (slot-value effective-slotdef 'persistence)
          (some #'slot-persistence persistent-slotdefs))

    ;; If exactly one direct slot is indexed, then the effective one is
    ;; too. If more then one is indexed, signal an error.
    (let ((index-slotdefs (remove-if-not #'slot-index persistent-slotdefs)))
      (cond ((cdr index-slotdefs)
             (error "Multiple indexes for slot ~S in ~S:~% ~{~S~^, ~}."
                    slot-name class
                    (mapcar #'slot-index index-slotdefs)))
            (index-slotdefs
             (setf (slot-value effective-slotdef 'index)
                   (slot-index (car index-slotdefs))))))
     
    ;; Return the effective slot definition.
    effective-slotdef))


