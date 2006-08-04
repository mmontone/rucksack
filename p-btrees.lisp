;; $Id: p-btrees.lisp,v 1.7 2006-08-04 22:04:43 alemmens Exp $

(in-package :rucksack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Btrees: API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
   ;; Btrees
   #:btree
   #:btree-key< #:btree-key<= #:btree-key= #:btree-key>= #:btree-key>
   #:btree-value=
   #:btree-max-node-size #:btree-unique-keys-p
   #:btree-key-type #:btree-value-type
   #:btree-node-class

   ;; Nodes
   #:btree-node

   ;; Functions
   #:btree-search #:btree-insert #:btree-delete #:map-btree

   ;; Conditions
   #:btree-error #:btree-search-error #:btree-insertion-error
   #:btree-key-already-present-error #:btree-type-error
   #:btree-error-btree #:btree-error-key #:btree-error-value
|#



#+nil(declaim (optimize (debug 3) (speed 0) (space 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; B-trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

This is a modified version of the in-memory btrees.  We use p-arrays,
p-conses and persistent-objects.

Basically, a B-tree is a balanced multi-way tree.

The reason for using multi-way trees instead of binary trees is that the nodes
are expected to be on disk; it would be inefficient to have to execute
a disk operation for each tree node if it contains only 2 keys.

The key property of B-trees is that each possible search path has the same
length, measured in terms of nodes.
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conditions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition btree-error (error)
  ((btree :initarg :btree :reader btree-error-btree)))

(define-condition btree-search-error (btree-error)
  ((key :initarg :key :reader btree-error-key))
  (:report (lambda (condition stream)
             (format stream "An entry for the key ~S could not be found."
                     (btree-error-key condition)))))


(define-condition btree-insertion-error (btree-error)
  ((key :initarg :key :reader btree-error-key)
   (value :initarg :value :reader btree-error-value)))

(define-condition btree-key-already-present-error (btree-insertion-error)
  ()
  (:report (lambda (condition stream)
             (format stream "There's already another value for the key ~S."
                     (btree-error-key condition)))))

(define-condition btree-type-error (btree-error type-error)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass btree ()
  ((key<   :initarg :key<   :reader btree-key<   :initform '<)
   (value= :initarg :value= :reader btree-value= :initform 'p-eql
           :documentation "This is only used for btrees with non-unique keys.")
   ;;
   (node-class :initarg :node-class
               :reader btree-node-class
               :initform 'btree-node)
   (max-node-size :initarg :max-node-size
                  :reader btree-max-node-size
                  :initform 100
                  :documentation "An integer specifying the preferred maximum number
of keys per btree node.")
   (unique-keys-p :initarg :unique-keys-p
                  :reader btree-unique-keys-p
                  :initform t
                  :documentation "If false, one key can correspond to more than one value.")
   (key-type :initarg :key-type
             :reader btree-key-type
             :initform t
             :documentation "The type of all keys.")
   (value-type :initarg :value-type
               :reader btree-value-type
               :initform t
               :documentation "The type of all values.")
   (root :accessor btree-root))
  (:metaclass persistent-class))


(defmethod initialize-instance :around ((btree btree)
                                        &rest initargs
                                        &key key< value=
                                        &allow-other-keys)
  ;; It must be possible to save these btrees in the cache, but
  ;; that will not work for function objects because they can't be
  ;; serialized. This means that you should only specify symbols that
  ;; name a function.  For program-independent databases you should
  ;; only use symbols from the COMMON-LISP package.
  (declare (ignore initargs))
  (if (and (symbolp key<) (symbolp value=))
    (call-next-method)
    (error "The :key< and :value= initargs for persistent btrees
must be symbols naming a function, otherwise they can't be saved on
disk.")))

;;
;; Comparison functions that can be deduced from KEY< (because the
;; btree keys have a total order).
;;

(defmethod btree-key= ((btree btree))
  (let ((key< (btree-key< btree)))
    (lambda (key1 key2)
      (and (not (funcall key< key1 key2))
           (not (funcall key< key2 key1))))))

(defmethod btree-key>= ((btree btree))
  (lambda (key1 key2)
    (funcall (btree-key< btree) key2 key1)))

(defmethod btree-key<= ((btree btree))
  (let ((key< (btree-key< btree)))
    (lambda (key1 key2)
      (or (funcall key< key1 key2)
          (not (funcall key< key2 key1))))))

(defmethod btree-key> ((btree btree))
  (let ((key< (btree-key< btree)))
    (lambda (key1 key2)
      (and (not (funcall key< key1 key2))
           (funcall key< key2 key1)))))


;;
;; The next two classes are for internal use only, so we don't bother
;; with fancy long names.
;;

(defclass btree-node ()
  ((index :initarg :index
          :initform '()
          :accessor btree-node-index
          :documentation "A vector of key/value pairs.  The keys are
sorted by KEY<. No two keys can be the same.  For leaf nodes of btrees
with non-unique-keys, the value part is actually a list of values.
For intermediate nodes, the value is a child node.  All keys in the
child node will be KEY<= the child node's key in the parent node.")
   (index-count :initform 0
                :accessor btree-node-index-count
                :documentation "The number of key/value pairs in the index vector.")
   (leaf-p :initarg :leaf-p :initform nil :reader btree-node-leaf-p))
  (:metaclass persistent-class))

;;
;; Bindings
;; 

(defun node-binding (node i)
  (let ((index (btree-node-index node)))
    (p-aref index i)))

(defun (setf node-binding) (binding node i)
  (setf (p-aref (btree-node-index node) i)
        binding))


(defun make-binding (key value)
  (p-cons key value))

(defun binding-key (binding)
  (p-car binding))

(defun (setf binding-key) (key binding)
  (setf (p-car binding) key))

(defun (setf binding-value) (value binding)
  (setf (p-cdr binding) value))

(defun binding-value (binding)
  (p-cdr binding))


(defun make-leaf-value (btree value)
  (if (btree-unique-keys-p btree)
      value
    (p-cons value '())))

;;
;;

(defmethod initialize-instance :after ((node btree-node)
                                       &key btree &allow-other-keys)
  (setf (btree-node-index node) (p-make-array (btree-max-node-size btree)
                                              :initial-element nil)
        (btree-node-index-count node) 0))


(defmethod print-object ((node btree-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "with ~D pairs" (btree-node-index-count node))))

;;
;; Debugging
;;

(defun display-node (node)
  (pprint (node-as-cons node)))

(defun node-as-cons (node)
  (loop with index = (btree-node-index node)
        with leaf-p = (btree-node-leaf-p node)
        for i below (btree-node-index-count node)
        for binding = (p-aref index i)
        collect (list (binding-key binding)
                      (if leaf-p
                        (binding-value binding)
                        (node-as-cons (binding-value binding))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric btree-search (btree key &key errorp default-value)
  (:documentation "Returns the value (or list of values, for btrees
that don't have unique keys) corresponding to KEY.  If the btree has
non-unique keys and no value is found, the empty list is returned.  If
the btree has unique keys and no value is found, the result depends on
ERRORP option: if ERRORP is true, a btree-search-error is signalled;
otherwise, DEFAULT-VALUE is returned."))

 
(defmethod btree-search (btree key &key (errorp t) (default-value nil))
  (restart-case 
      (if (slot-boundp btree 'root)
          (node-search btree (btree-root btree) key errorp default-value)
        (not-found btree key errorp default-value))
    (use-value (value)
      :report (lambda (stream)
                (format stream "Specifiy a value to use this time for key ~S." key))
      :interactive (lambda ()
                     (format t "Enter a value for key ~S: " key)
                     (multiple-value-list (eval (read))))
      value)
    (store-value (value)
      :report (lambda (stream)
                (format stream "Specify a value to set key ~S to." key))
      :interactive (lambda ()
                     (format t "Enter a value for key ~S: " key)
                     (multiple-value-list (eval (read))))
      (btree-insert btree key value))))


(defun not-found (btree key errorp default-value)
  (if (btree-unique-keys-p btree)
      (if errorp
          (error 'btree-search-error :btree btree :key key)
        default-value)
    '()))

;;
;; Node-search
;;

(defun find-binding-in-node (key node btree)
  (let ((index-count (btree-node-index-count node)))
    (and (plusp index-count)
         (loop with array = (btree-node-index node)
               with btree-key< = (btree-key< btree)
               for i from 0 below index-count
               for candidate = (p-aref array i)
               for candidate-key = (binding-key candidate)
               while (funcall btree-key< candidate-key key)
               finally (when (funcall (btree-key= btree) key candidate-key)
                         (return candidate))))))
  
(defgeneric node-search (btree node key errorp default-value)
  (:method ((btree btree) (node btree-node) key errorp default-value)
   (if (btree-node-leaf-p node)
       (let ((binding (find-binding-in-node key node btree)))
         (if binding
             (binding-value binding)
           (not-found btree key errorp default-value)))
     (let ((subnode (find-subnode btree node key)))
       (node-search btree subnode key errorp default-value)))))


(defun find-subnode (btree node key)
  "Returns the subnode that contains more information for the given key."
  ;; Find the first binding with a key >= the given key and return
  ;; the corresponding subnode.
  ;; EFFICIENCY: We should probably use binary search for this.
  (loop with btree-key< = (btree-key< btree)
        with last-index = (1- (btree-node-index-count node))
        for i to last-index
        for binding = (node-binding node i)
        when (or (= i last-index)
                 (funcall btree-key< key (binding-key binding))
                 (not (funcall btree-key< (binding-key binding) key)))
        do (return-from find-subnode (binding-value binding)))
  (error "This shouldn't happen."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Insert
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric btree-insert (btree key value &key if-exists))

(defmethod btree-insert ((btree btree) key value &key (if-exists :overwrite))
  ;; Check that key and value are of the right type.
  (unless (typep key (btree-key-type btree))
    (error 'btree-type-error
           :btree btree
           :datum key
           :expected-type (btree-key-type btree)))
  (unless (typep value (btree-key-type btree))
    (error 'btree-type-error
           :btree btree
           :datum value
           :expected-type (btree-value-type btree)))
  ;; Do the real work.
  (if (slot-boundp btree 'root)
      (btree-node-insert btree (btree-root btree) (list nil) key value if-exists)
    ;; Create a root.
    (let ((leaf (make-instance (btree-node-class btree)
                               :btree btree
                               :leaf-p t)))
      (insert-new-binding leaf 0 key (make-leaf-value btree value))
      (let* ((empty-leaf (make-instance (btree-node-class btree)
                                        :btree btree
                                        :leaf-p t))
             (root (make-root btree key leaf 'key-irrelevant empty-leaf)))
        (setf (btree-root btree) root))))
  ;; Return the inserted value.
  value)

(defun check-btree (btree)
  ;; Check that it is completely sorted.
  (let (prev-key)
    (map-btree btree
               (lambda (key value)
                 (declare (ignore value))
                 (when prev-key
                   (unless (funcall (btree-key< btree) prev-key key)
                     (display-node (btree-root btree))
                     (error "Btree inconsistency between ~D and ~D" prev-key key)))
                 (setq prev-key key)))))
                   

(defun make-root (btree left-key left-subnode right-key right-subnode)
  (let* ((root (make-instance (btree-node-class btree) :btree btree)))
    (setf (node-binding root 0) (make-binding left-key left-subnode)
          (node-binding root 1) (make-binding right-key right-subnode)
          (btree-node-index-count root) 2)
    root))


;;
;; Node insert
;;

(defgeneric btree-node-insert (btree node parent-stack key value if-exists))

(defmethod btree-node-insert ((btree btree) (node btree-node) parent-stack key value if-exists)
  (cond ((btree-node-leaf-p node)
         (leaf-insert btree node parent-stack key value if-exists))
        (t (let ((subnode (find-subnode btree node key)))
             (btree-node-insert btree subnode (cons node parent-stack) key value if-exists)))))

(defun smallest-key (node)
  (if (btree-node-leaf-p node)
      (binding-key (node-binding node 0))
    (smallest-key (binding-value (node-binding node 0)))))

(defun biggest-key (node)
  (if (btree-node-leaf-p node)
      (binding-key (node-binding node (1- (btree-node-index-count node))))
    (biggest-key (binding-value (node-binding node (1- (btree-node-index-count node)))))))


(defun split-btree-node (btree node parent-stack key)
  ;; The node is (almost) full.
  ;; Create two new nodes and divide the current node-index over
  ;; these two new nodes.
  (let* ((split-pos (floor (btree-node-index-count node) 2))
         (left (make-instance (btree-node-class btree)
                              :btree btree
                              :leaf-p (btree-node-leaf-p node)))
         (right (make-instance (btree-node-class btree)
                               :btree btree
                               :leaf-p (btree-node-leaf-p node))))
    ;; Divide the node over the two new nodes.
    (p-replace (btree-node-index left) (btree-node-index node)
               :end2 split-pos)
    (p-replace (btree-node-index right) (btree-node-index node)
               :start2 split-pos)
    (setf (btree-node-index-count left) split-pos
          (btree-node-index-count right) (- (btree-node-index-count node) split-pos))
    ;;
    (let ((left-key
           ;; The key that splits the two new nodes.
           (biggest-key left)))
      (cond ((p-eql node (btree-root btree))
             ;; Make a new root.
             (setf (btree-root btree)
                   (make-root btree left-key left 'key-irrelevant right)))
            (t
             (let* ((parent (first parent-stack))
                    (node-pos (node-position node parent))
                    (parent-binding (node-binding parent node-pos))
                    (old-key (binding-key parent-binding)))
               (when (node-full-p btree parent)
                 (setq parent (split-btree-node btree parent (rest parent-stack) old-key)
                       node-pos (node-position node parent)))
               ;; Replace the original subnode by the left-child and
               ;; add a new-binding with new-key & right-child.
               (setf (binding-key parent-binding) left-key
                     (binding-value parent-binding) left)
               ;; Insert a new binding for the right node.
               (insert-new-binding parent (1+ node-pos) old-key right))))
      ;; Return the node that's relevant for KEY
      (if (or (eq key 'key-irrelevant)
              (funcall (btree-key< btree) left-key key))
          right
        left))))


(defun node-position (node parent)
  (p-position node (btree-node-index parent)
                   :key #'binding-value
                   :end (btree-node-index-count parent)))


(defun insert-new-binding (node position key value)
  ;; This function must only be called if we know that the index isn't
  ;; full already
  (unless (>= position (btree-node-index-count node))
    ;; Make room by moving bindings to the right.
    (let ((node-index (btree-node-index node))
          (length (btree-node-index-count node)))
      (p-replace node-index node-index
                 :start1 (1+ position) :end1 (1+ length)
                 :start2 position :end2 length)))
  ;; Insert new binding.
  (setf (node-binding node position) (make-binding key value))
  (incf (btree-node-index-count node)))


;;
;; Debugging
;;

(defun check-node (btree node)
  (loop for i below (1- (btree-node-index-count node))
        for left-key = (binding-key (node-binding node i))
        for right-key = (binding-key (node-binding node (1+ i)))
        do (unless (or (eql right-key 'key-irrelevant)
                       (funcall (btree-key< btree) left-key right-key))
             (display-node node)
             (error "Inconsistent node ~S" node))))


;;
;; Leaf insert
;;

(defun leaf-insert (btree leaf parent-stack key value if-exists)
  (let ((binding (find-binding-in-node key leaf btree)))
    (if binding
        ;; Key already exists.
        (if (btree-unique-keys-p btree)
            (ecase if-exists
              (:overwrite
               (setf (binding-value binding) value))
              (:error
               ;; Signal an error unless the old value happens to be
               ;; the same as the new value.
               (unless (funcall (btree-value= btree) (binding-value binding) value)
                 (error 'btree-key-already-present-error
                        :btree btree
                        :key key
                        :value value))))
          ;; For non-unique keys, we ignore the :if-exists options and
          ;; just add value to the list of values (unless value is already
          ;; there).
          (unless (p-find value (binding-value binding) :test (btree-value= btree))
            (setf (binding-value binding)
                  (p-cons value (binding-value binding)))))
       ;; The key doesn't exist yet. Create a new binding and add it to the
       ;; leaf index in the right position.
       (progn
        (when (node-full-p btree leaf)
          (setq leaf (split-btree-node btree leaf parent-stack key)))
        (let ((new-position (p-position key (btree-node-index leaf)
                                        :test (btree-key< btree)
                                        :key #'binding-key
                                        :end (btree-node-index-count leaf))))
          (insert-new-binding leaf
                              (or new-position (btree-node-index-count leaf))
                              key
                              (make-leaf-value btree value)))))))
  
(defun node-full-p (btree node)
  (>= (btree-node-index-count node) (btree-max-node-size btree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Delete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defgeneric btree-delete (btree key &key if-does-not-exist))
 
(defmethod btree-delete ((btree btree) key &key (if-does-not-exist :ignore))
  (if (slot-boundp btree 'root)
    (btree-node-delete btree (btree-root btree) (list nil) key if-does-not-exist)
    (ecase if-does-not-exist
      (:ignore)
      (:error (error 'btree-search-error :btree btree :key key)))))

(defgeneric btree-node-delete (btree node parent-stack key if-does-not-exist))

(defmethod btree-node-delete ((btree btree) (node btree-node) parent-stack key if-does-not-exist)
  (cond ((btree-node-leaf-p node)
         (leaf-delete btree node parent-stack key if-does-not-exist))
        (t (let ((subnode (find-subnode btree node key)))
             (btree-node-delete btree subnode (cons node parent-stack) key if-does-not-exist)))))
 
(defun leaf-delete (btree leaf parent-stack key if-does-not-exist)
  (let ((binding (find-binding-in-node key leaf btree)))
    (unless binding
      (ecase if-does-not-exist
        (:ignore (return-from leaf-delete))
        (:error (error 'btree-search-error :btree btree :key key))))
    (remove-key leaf key)
    (unless (node-full-enough-p btree leaf)
      (enlarge-node btree leaf parent-stack))))

(defun enlarge-node (btree node parent-stack)
  (let ((parent (first parent-stack)))
    ;; Don't enlarge root node.
    (unless parent
      (return-from enlarge-node))
    (let ((node-pos (node-position node parent))
          left-sibling)
      (when (plusp node-pos)
        (setq left-sibling (binding-value (node-binding parent (1- node-pos))))
        (unless (node-has-min-size-p btree left-sibling)
          (distribute-nodes left-sibling node parent)
          (return-from enlarge-node)))
      (when (< (1+ node-pos) (btree-node-index-count parent))
        (let ((right-sibling (binding-value (node-binding parent (1+ node-pos)))))
          (unless (node-has-min-size-p btree right-sibling)
            (distribute-nodes node right-sibling parent)
            (return-from enlarge-node))
          (join-nodes btree node right-sibling parent-stack)
          (return-from enlarge-node)))
      (when left-sibling
        (join-nodes btree left-sibling node parent-stack)
        (return-from enlarge-node))))
  (error "This should not happen."))

(defun distribute-nodes (left-node right-node parent)
  ;; One of LEFT-NODE and RIGHT-NODE doesn't have enough elements, but
  ;; the union of both has enough elements for two nodes, so we
  ;; distribute the elements between the two nodes.
  (let* ((left-index (btree-node-index left-node))
         (left-length (btree-node-index-count left-node))
         (right-index (btree-node-index right-node))
         (right-length (btree-node-index-count right-node))
         (sum (+ left-length right-length))
         (median (floor sum 2)))
    ;; LEFT-NODE will have MEDIAN elements, RIGHT-NODE will have
    ;; (- SUM MEDIAN) elements.
    (cond ((< left-length median)
           ;; Case 1: move some elements to the left.
           (p-replace left-index right-index
                      :start1 left-length
                      :start2 0 :end2 (- median left-length))
           (p-replace right-index right-index
                      :start1 0
                      :start2 (- median left-length) :end2 right-length))
          ((> left-length median)
           ;; Case 2: move some elements to the right.
           (p-replace right-index right-index
                      :start1 (- left-length median)
                      :start2 0 :end2 right-length)
           (p-replace right-index left-index
                      :start1 0
                      :start2 median :end2 left-length)))
    ;; Set new lengths for both nodes.
    (shorten left-node median)
    (shorten right-node (- sum median))
    ;; Select new separator key.
    (setf (binding-key (node-binding parent (node-position left-node parent)))
          (biggest-key left-node))))

(defun join-nodes (btree left-node right-node parent-stack)
  ;; Create one node which contains the elements of both LEFT-NODE and
  ;; RIGHT-NODE.
  (let* ((parent (first parent-stack))
         (left-index (btree-node-index left-node))
         (left-length (btree-node-index-count left-node))
         (right-index (btree-node-index right-node))
         (right-length (btree-node-index-count right-node))
         (left-position (node-position left-node parent))
         (left-binding (node-binding parent left-position))
         (right-binding (node-binding parent (1+ left-position))))
    ;; Move all elements into LEFT-NODE.
    (p-replace left-index right-index
               :start1 left-length
               :start2 0 :end2 right-length)
    ;; Remove key which pointed to LEFT-NODE.
    (remove-key parent (binding-key left-binding))
    ;; Make binding which pointed to RIGHT-NODE point to LEFT-NODE.
    (setf (binding-value right-binding) left-node)
    ;; Set new length of LEFT-NODE.
    (setf (btree-node-index-count left-node) (+ right-length left-length))
    ;; Check if we have to enlarge the parent as well because we
    ;; removed one key.
    (unless (node-full-enough-p btree parent)
      (enlarge-node btree parent (rest parent-stack)))))

(defun shorten (node new-length)
  ;; Set length of NODE to NEW-LENGTH, set bindings behind NEW-LENGTH
  ;; to NIL, so the GC can throw them away.
  (loop for i from new-length below (btree-node-index-count node)
        do (setf (node-binding node i) nil))
  (setf (btree-node-index-count node) new-length))

(defun remove-key (node key)
  (let ((position (key-position key node))
        (length (btree-node-index-count node)))
    (unless (>= position (1- length))
      ;; Move bindings to the left.
      (let ((node-index (btree-node-index node)))
        (p-replace node-index node-index
                   :start1 position :end1 (1- length)
                   :start2 (1+ position) :end2 length)))
    (shorten node (1- length))))
    
(defun key-position (key node)
  (p-position key (btree-node-index node)
              :key #'binding-key
              :end (btree-node-index-count node)))

(defun node-full-enough-p (btree node)
  (>= (btree-node-index-count node)
      (floor (btree-max-node-size btree) 2)))

(defun node-has-min-size-p (btree node)
  (= (btree-node-index-count node)
     (floor (btree-max-node-size btree) 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Iterating
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric map-btree (btree function
                             &key min max include-min include-max order)
  (:documentation "Calls FUNCTION for all key/value pairs in the btree where key
is in the specified interval. FUNCTION must be a binary function; the first
argument is the btree key, the second argument is the btree value (or list of
values, for btrees with non-unique keys).

MIN, MAX, INCLUDE-MIN and INCLUDE-MAX specify the interval.  The interval is
left-open if MIN is nil, right-open if MAX is nil.  The interval is inclusive
on the left if INCLUDE-MIN is true (and exclusive on the left otherwise).
The interval is inclusive on the right if INCLUDE-MAX is true (and exclusive
on the right otherwise).

ORDER is either :ASCENDING (default) or :DESCENDING."))

(defmethod map-btree ((btree btree) function
                      &key min max include-min include-max (order :ascending))
  (when (slot-boundp btree 'root)
    (map-btree-node btree (slot-value btree 'root) function
                    min max include-min include-max order)))

(defgeneric map-btree-node (btree node function min max include-min include-max order)
  ;; DO: This only works when MIN, MAX, INCLUDE-MIN and INCLUDE-MAX are all nil.
  ;; Implement the general case. Also implement the descending order.
  (:method ((btree btree) (node btree-node) function min max include-min include-max order)
   (loop for i below (btree-node-index-count node)
         do (let ((binding (node-binding node i)))
              (if (btree-node-leaf-p node)
                  (funcall function (binding-key binding) (binding-value binding))
                (map-btree-node btree (binding-value binding)
                                function min max include-min include-max order))))))
