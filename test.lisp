;;; Todo: change, this is probably stupid
(load "trie.lisp")


(defclass word-token ()
  ((token
    :accessor token
    :initarg :token
    :initform nil)
   (data
    :accessor data
    :initarg :data
    :initform nil)))

(defparameter *nouns* (make-trie-from-file "nouns.txt"))
(defparameter *verbs* (make-trie-from-file "verbs.txt"))
(defparameter *adverbs* (make-trie-from-file "adverbs.txt"))
(defparameter *adjectives* (make-trie-from-file "adjectives.txt"))


(defun tokenize-word (x)
  (cond
    ((trie-member x *nouns*)
     (make-instance 'word-token
                    :token :noun
                    :data x))
    ((trie-member x *verbs*)
     (make-instance 'word-token
                    :token :verb
                    :data x))
    ((trie-member x *adverbs*)
     (make-instance 'word-token
                    :token :adverb
                    :data x))
    ((trie-member x *adjectives*)
     (make-instance 'word-token
                    :token :adjective
                    :data x))
    (t
     (make-instance 'word-token
                    :token :unknown
                    :data x))))


(defun print-token (tok)
  (format t "~a, ~a~%" (token tok) (data tok)))

(defun split-on-space (x)
  (let ((xl (coerce x 'list))
        (l nil)
        (cur ""))
    (loop for i in xl do
         (cond
           ((eql i #\Space)
            (setf l (append l (list cur)))
            (setf cur ""))
           (t
            (setf cur (concatenate 'string cur (string i))))))
    (if (not (equal cur ""))
        (setf l (append l (list cur))))
    l))

(let* ((x (read-line))
       (list (split-on-space x))
       (tokens (map 'list #'tokenize-word list)))
  (map nil #'print-token tokens))
