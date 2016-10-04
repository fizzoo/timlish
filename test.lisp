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

(defparameter *adjectives* (make-trie-from-file "wordlists/adjectives.txt"))
(defparameter *adverbs* (make-trie-from-file "wordlists/adverbs.txt"))
(defparameter *articles* (make-trie-from-file "wordlists/articles.txt"))
(defparameter *conjugations* (make-trie-from-file "wordlists/conjugations.txt"))
(defparameter *interjections* (make-trie-from-file "wordlists/interjections.txt"))
(defparameter *nouns* (make-trie-from-file "wordlists/nouns.txt"))
(defparameter *prepositions* (make-trie-from-file "wordlists/prepositions.txt"))
(defparameter *pronouns* (make-trie-from-file "wordlists/pronouns.txt"))
(defparameter *verbs* (make-trie-from-file "wordlists/verbs.txt"))

(let ((tries-to-word (list
                      (cons *articles* :articles)
                      (cons *conjugations* :conjugations)
                      (cons *pronouns* :pronouns)
                      (cons *prepositions* :prepositions)
                      (cons *adverbs* :adverbs)
                      (cons *adjectives* :adjectives)
                      (cons *nouns* :nouns)
                      (cons *interjections* :interjections)
                      (cons *verbs* :verbs))))
  (defun tokenize-word (x)
    "Tokenize the word by checking which class it belongs to."
    (let ((tok (cdr (find-if
                     (lambda (a) (trie-member (string-downcase x) (car a)))
                     tries-to-word))))
      (if (not tok)
          (setf tok (class-of-char (char x 0))))
      (make-instance 'word-token
                     :token tok
                     :data x))))


(defun print-token (tok)
  "Print a token by printing its token and data."
  (format t "~12a: ~a~%" (token tok) (data tok)))

(defun class-test-alpha (x)
  "Tests if character x belongs to class alpha."
  (or (alpha-char-p x)
      (eql x #\')))
(defun class-test-punc (x)
  "Tests if character x belongs to class punc."
  (member x (coerce ".,?!" 'list)))
;;; Possibly unnecessary, 
;; (defun class-test-whitespace (x)
;;   (member x (list #\Space)))            ;May add tab/newline

(defun make-nice-string ()
  "Make an empty string that's adjustable with fill-pointer."
  (make-array 0 :element-type 'character :adjustable t :fill-pointer t))

(defun class-of-char (x)
  "Get the 'character-class' of character x."
  (cond ((class-test-alpha x)
         :alpha)
        ((class-test-punc x)
         :punctuation)
        (t
         :whitespace)))

(defun split-to-tokenizable (x)
  "Split the string into a list of tokenizable 'words'.
The elements of this list all belong to the same class, so you
could get the character-class through (class-of-char (char r 0))"
  (let ((l nil)
        (cur (make-nice-string))
        (last-class (class-of-char (char x 0))))
    (loop for i across x do
         (let ((cur-class (class-of-char i)))
           (cond
             ((eq cur-class last-class)
              (vector-push-extend i cur))
             (t
              (setf l (append l (list cur)))
              (setf cur (make-nice-string))
              (setf last-class cur-class)
              (vector-push-extend i cur)))))
    (if (not (equal cur ""))
        (setf l (append l (list cur))))
    l))

;;; The testing
(let* ((x "I am victor. This is good.")
       (list (split-to-tokenizable x))
       (tokens (map 'list #'tokenize-word list)))
  (map nil #'print-token tokens))
