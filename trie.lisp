(defclass trie ()
  ((letter
    :initarg :letter
    :type character
    :initform nil)
   (eow
    :initform nil)
   (children
    :initform nil))
  (:documentation "A trie class."))

(defun trie-child-find (c trie)
  "Find the tree with letter c of the children of trie, if one exists."
  (loop for l in (slot-value trie 'children) do
       (if (eql c (slot-value l 'letter))
           (return l))))

(defun trie-add (str trie)
  "Modify trie to also contain str."
  (with-slots (letter eow children) trie
    (let ((s (coerce str 'list)))
      (if (not s)
          (setf eow t)
          (let ((n (trie-child-find (car s) trie)))
            (cond (n
                   (trie-add (cdr s) n))
                  (t
                   (setf children
                         (cons (make-instance 'trie :letter (car s))
                               children))
                   (trie-add (cdr s)
                             (car children)))))))))

(defun trie-member (str trie)
  "Check if trie contains str."
  (with-slots (letter eow children) trie
    (let* ((s (coerce str 'list))
           (fc (trie-child-find (car s) trie)))
      (cond ((not s)
             eow)
            (fc
             (trie-member (cdr s) fc))
            (t
             nil)))))

(defun trie-print (trie)
  "Print trie sort of prettily."
  (with-slots (letter eow children) trie
    (format t "~%(~a . ~a . " eow letter)
    (loop for i in children do
         (trie-print i))
    (format t ")")))

(defun make-trie-from-file (filename)
  "Create a trie from a file, adding each line as a word."
  (let ((tr (make-instance 'trie)))
    (with-open-file (s filename)
      (loop for line = (read-line s nil :eof)
         until (eq line :eof)
         do (trie-add line tr)))
    tr))

