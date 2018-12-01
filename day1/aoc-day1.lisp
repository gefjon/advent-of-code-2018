(defpackage day1
  (:use :cl)
  (:export :part1 :part2))

(in-package :day1)

(defvar *frequency-change-regexp* (ppcre:create-scanner "[+-]\\d+"))

(defun part1 (change-string)
  (sum-frequency-recursive (change-string-to-list change-string) 0))

(defun change-string-to-list (change-string)
  (map 'list
       #'parse-integer
       (ppcre:all-matches-as-strings *frequency-change-regexp* change-string)))

(defun sum-frequency-recursive (change-list sum)
  (if change-list
      (sum-frequency-recursive (cdr change-list)
                               (+ (car change-list) sum))
      sum))

(defun find-repeated-frequency
    (&key complete-list remaining-list visited-frequencies current-frequency)
  ; If CURRENT-FREQUENCY is in VISITED-FREQUENCIES, return it
  (if (binary-tree-contains visited-frequencies current-frequency)
      current-frequency

      ; Otherwise, advance by one step and recurse
      (find-repeated-frequency
       :complete-list complete-list
       :current-frequency (+ (car remaining-list) current-frequency)
       
       ; If REMAINING-LIST is empty, start again at the start of COMPLETE-LIST
       :remaining-list (or (cdr remaining-list) complete-list)

       ; BINARY-TREE-INSERT is a little weird - like NREVERSE, it may
       ; modify the tree in place, but the return value should be
       ; used, not the original input
       :visited-frequencies (binary-tree-insert visited-frequencies current-frequency))))

; A simple binary tree of integers
(defclass binary-tree ()
  ((value :initarg :value
          :type 'integer
          :accessor binary-tree-value)
   (left :initarg :left
         :initform nil
         :type '(or binary-tree null)
         :accessor binary-tree-left)
   (right :initarg :right
          :initform nil
          :type '(or binary-tree null)
          :accessor binary-tree-right)))

(defun binary-tree-leaf (element)
  "Construct a binary tree with no child nodes"
  (make-instance 'binary-tree :value element))

(defun binary-tree-contains (tree element)
  "T iff TREE contains ELEMENT"
  (and tree (with-slots (value left right) tree
              (cond ((= element value) t)
                    ((< element value) (binary-tree-contains left element))
                    ((> element value) (binary-tree-contains right element))
                    (t nil)))))

(defun binary-tree-insert (tree element)
  "Insert ELEMENT into TREE

May or may not modify TREE in-place; use the returned BINARY-TREE rather than the input"
  ; If passed NIL (the empty tree), insertion is trivial - return a
  ; new tree of one member. Unfortunately, doing so will not affect
  ; TREE in the caller's environment - that's why only the return
  ; value of this function should be used.
  (if (not tree)
      (binary-tree-leaf element)
      ; Otherwise, insert the element in-place, altering the tree. For
      ; consistent behavior, return the altered tree.
      (progn (binary-tree-insert-internal tree element)
             tree)))

(defun binary-tree-insert-internal (tree element)
  "The real algorithm behind BINARY-TREE-INSERT"
  ; Note: this function is recursive on itself, not
  ; alternating-recursive with BINARY-TREE-INSERT. That should enable
  ; tail-call optimization, but implementations may not take advantage
  ; of it.
  (with-slots (value left right) tree
        (cond ((= element value)
               (error "may not insert repeated element"))
              
              ((< element value)
               (if left
                   (binary-tree-insert left element)
                   (setf (binary-tree-left tree) (binary-tree-leaf element))))
              
              ((> element value)
               (if right
                   (binary-tree-insert right element)
                   (setf (binary-tree-right tree) (binary-tree-leaf element)))))))

(defun part2 (change-string)
  (let ((change-list (change-string-to-list change-string)))
    (find-repeated-frequency
     :complete-list change-list
     :remaining-list change-list
     :visited-frequencies (binary-tree-leaf 0)
     :current-frequency 0)))
