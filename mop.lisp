;; $Id: mop.lisp,v 1.1 2006-05-16 21:16:34 alemmens Exp $ $Date: 2006-05-16 21:16:34 $

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
(for the standard class index).  Default value is NIL.")))

(defclass persistent-slot-mixin ()
  ((persistence :initarg :persistence
                :initform t
                :reader slot-persistence
                :documentation "T for persistent slots, NIL for
transient slots.  Default value is T.")
   (index :initarg :index
          :initform nil
          :reader slot-index)))

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
;; PERSISTENT-CLASS.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+lispworks
(defmethod clos:process-a-slot-option ((class persistent-class)
                                       option
                                       value
                                       already-processed-options
                                       slot)
  (if (member option '(:index :persistence))
      (list* option value already-processed-options)
    (call-next-method)))

#+lispworks
(defmethod clos:process-a-class-option ((class persistent-class)
                                        option-name
                                        value)
  (if (member value '(:index))
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
    (ensure-class-schema class)
    result))

(defmethod reinitialize-instance :around ((class persistent-class)
                                          &rest args
                                          &key direct-superclasses
                                          &allow-other-keys)
  ;; This is a copy of the code for initialize-instance at the moment.
  (let ((result (apply #'call-next-method
                       class
                       :direct-superclasses (maybe-add-persistent-object-class
                                             class
                                             direct-superclasses)
                       ;; Tell Lispworks that it shouldn't bypass
                       ;; slot-value-using-class.
                       #+lispworks :optimize-slot-access #+lispworks nil
                       args)))
    (ensure-class-schema class)
    result))


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

(defun ensure-class-schema (class)
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
        (rucksack-update-slot-indexes rucksack class))))
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

  ;; Compute the effective slot definition for slots in a
  ;; persistent-class.  We use a simple strategy at the moment:
  ;; just use the most specific direct slot definition and ignore
  ;; all others (usually there aren't any others anyway).

  (declare (ignore slot-name))
  (let ((effective-slot-def (call-next-method))
        (direct-slot-def (first direct-slot-definitions)))
    
    ;; NOTE: A persistent-class may also contain slots of another type
    ;; than persistent-direct-slot-definition. (For instance, when
    ;; we combine the persistent-class metaclass with another one.)
    ;; Those other slot definitions should not be touched here.
    
    (when (typep direct-slot-def 'persistent-direct-slot-definition)
      
      ;; Just copy the values of 'our' slot options from the
      ;; direct-slot-definition to the effective-slot-definition.
      (dolist (option '(persistence index))
        (when (slot-boundp direct-slot-def option)
          (setf (slot-value effective-slot-def option)
                (slot-value direct-slot-def option)))))

    ;; Return the effective slot definition.
    effective-slot-def))


