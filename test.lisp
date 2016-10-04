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


(let ((tries-to-word (list (cons *verbs* :verb)
                           (cons *adverbs* :adverb)
                           (cons *nouns* :noun)
                           (cons *adjectives* :adjective))))
  (defun tokenize-word (x)
    (let ((tok (cdr (find-if
                     (lambda (a) (trie-member (string-downcase x) (car a)))
                     tries-to-word))))
      (if tok
          (make-instance 'word-token
                         :token tok
                         :data x)
          (make-instance 'word-token
                         :token :unknown
                         :data x)))))


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

(let* ((x "Hi my name is tim guy")
       (list (split-on-space x))
       (tokens (map 'list #'tokenize-word list)))
  (map nil #'print-token tokens))
