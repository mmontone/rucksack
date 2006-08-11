;; $Id: index.lisp,v 1.5 2006-08-11 12:44:21 alemmens Exp $

(in-package :rucksack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indexing: API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric map-index (index function
                       &key equal min max include-min include-max order)
  (:documentation "Calls FUNCTION for all key/value pairs in the btree
where key is in the specified interval. FUNCTION must be a binary
function; the first argument is the index key, the second argument is
the index value (or list of values, for indexes with non-unique keys).

If EQUAL is specified, the other arguments are ignored; the function
will be called once (if there is a key with the same value as EQUAL)
or zero time (if there is no such key).

MIN, MAX, INCLUDE-MIN and INCLUDE-MAX specify the interval.  The
interval is left-open if MIN is nil, right-open if MAX is nil.  The
interval is inclusive on the left if INCLUDE-MIN is true (and
exclusive on the left otherwise).  The interval is inclusive on the
right if INCLUDE-MAX is true (and exclusive on the right otherwise).

ORDER is either :ASCENDING (default) or :DESCENDING."))

(defgeneric index-insert (index key value &key if-exists)
  (:documentation
 "Insert a key/value pair into an index.  IF-EXISTS can be either
:OVERWRITE (default) or :ERROR."))

(defgeneric index-delete (index key value &key if-does-not-exist)
  (:documentation
 "Remove a key/value pair from an index.  IF-DOES-NOT-EXIST can be
either :IGNORE (default) or :ERROR."))

;; make-index (index-spec unique-keys-p) [Function]

;; index-spec-equal (index-spec-1 index-spec-2) [Function]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indexing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod map-index ((index btree) function
                      &rest args
                      &key equal min max include-min include-max
                      (order :ascending))
  (declare (ignorable equal min max include-min include-max))
  (apply #'map-btree index function :order order args))


(defmethod index-insert ((index btree) key value &key (if-exists :overwrite))
  (btree-insert index key value :if-exists if-exists))

(defmethod index-delete ((index btree) key value
                         &key (if-does-not-exist :ignore))
  (btree-delete index key value :if-does-not-exist if-does-not-exist))

;;
;; Index specs
;;

;; An index spec is a symbol or a list starting with a symbol
;; and followed by a plist of keywords and values.
;; Examples: BTREE, (BTREE :KEY< <  :VALUE= P-EQL)


(defun make-index (index-spec unique-keys-p)
  ;; NOTE: All index classes must accept the :UNIQUE-KEYS-P initarg.
  (if (symbolp index-spec)
      (make-instance index-spec :unique-keys-p unique-keys-p)
    (apply #'make-instance
           (first index-spec)
           :unique-keys-p unique-keys-p
           (rest index-spec))))

(defun index-spec-equal (index-spec-1 index-spec-2)
  "Returns T iff two index specs are equal."
  (flet ((plist-subset-p (plist-1 plist-2)
           (loop for (key value) on plist-1 by #'cddr
                 always (equal (getf plist-2 key) value))))
    (or (eql index-spec-1 index-spec-2)
        (and (listp index-spec-1)
             (listp index-spec-2)
             (eql (first index-spec-1)
                  (first index-spec-2))
             (plist-subset-p (rest index-spec-1) (rest index-spec-2))
             (plist-subset-p (rest index-spec-2) (rest index-spec-1))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defining index specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)

  ;;
  ;; Defining index specs
  ;;

  (defparameter *index-specs*
    (make-hash-table))

  (defun define-index-spec (name spec &key (if-exists :overwrite))
    "NAME must be a keyword.  SPEC must be an index spec.  IF-EXISTS must be
either :OVERWRITE (default) or :ERROR."
    (assert (member if-exists '(:overwrite :error)))
    (when (eql if-exists :error)
      (let ((existing-spec (gethash name *index-specs*)))
        (when (and existing-spec
                   (not (index-spec-equal existing-spec spec)))
          (error "Index spec ~S is already defined.  Its definition is: ~S."
                 name existing-spec))))
    (setf (gethash name *index-specs*) spec))
  
  (defun find-index-spec (name &key (errorp t))
    (or (gethash name *index-specs*)
        (and errorp
             (error "Can't find index spec called ~S." name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predefined index specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun trim-whitespace (string)
    (string-trim '(#\space #\tab #\return #\newline) string))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (define-index-spec :number-index
                     '(btree :key< < :value= p-eql))

  (define-index-spec :string-index
                     '(btree :key< string< :value p-eql))

  (define-index-spec :symbol-index
                     '(btree :key< string< :value p-eql))

  (define-index-spec :case-insensitive-string-index
                     '(btree :key< string-lessp :value p-eql))

  (define-index-spec :trimmed-string-index
                     ;; Like :STRING-INDEX, but with whitespace trimmed left
                     ;; and right.
                     '(btree :key< string<
                             :key-key trim-whitespace
                             :value p-eql)))
