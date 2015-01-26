(in-package :cl-user)
(defpackage codos.markup
  (:use :cl :cl-markup :cl-ppcre)
  (:export
   :format-post))

(in-package :codos.markup)

(defun parse-token (token)
  "foo -> foo, [foo bar] -> (foo bar), [[foo] -> [foo "
  (cond
    ((or (zerop (length token))
         (not (and (char= (char token 0) #\[)
                   (char= (char token (1- (length token))) #\]))))
     token)
    ((char= (char token 1) #\[)
     (subseq token 1 (- (length token) 1)))
    (t (or (split "\\s+" (subseq token 1 (- (length token) 1))) ""))))

(defun preparse-post (text)
  "Find all tokens of the form [...] in the text and construct a list
  of them and intermediate text" 
  (mapcar #'parse-token (split "(\\[[^[]*])" text :with-registers-p t)))

(defun find-matching-token (list)
  "Return a list of the form (((token . args) text-in-token-scope) remaining-text)"
  (let ((token-name (caar list)))
    (loop for (token . rest) on list
       until (and (listp token) (string-equal token-name (subseq (car token) 1)))
         collecting token into token-scope
         finally (return (cons token-scope rest)))))

(defun parse-post (text)
  "Produces a parse tree out of the text"
  (labels ((parse (list)
             (cond
               ((endp list) nil)
               ((listp (car list))
                (destructuring-bind ((token &rest inside) &rest outside)
                    (find-matching-token list)
                  (cons (cons token (parse inside)) (parse outside))))
               (t (cons (car list) (parse (cdr list)))))))
    (parse (preparse-post text))))
      
(defun replace-line-breaks (str &optional (out *standard-output*))
  (loop for x across str
     if (char= x #\Newline) do (princ "<br/>" out)
     else do (write-char x out)))

(defclass tag ()
  ((args :initarg :args :reader tag-args)
   (content :initarg :content :reader tag-content)))

(defgeneric format-tag (tag stream)
  (:documentation "print the tag to stream")
  (:method (tag stream)
    (format-tree (tag-content tag) stream)))

(defclass simple-tag (tag)
  ((html-tag :initarg :tag :reader html-tag)))

(defgeneric html-attrs (tag)
  (:documentation "create html tag attrs")
  (:method (tag) ""))

(defmethod format-tag :around ((tag simple-tag) stream)
  (format stream "<~a ~a>" (html-tag tag) (html-attrs tag))
  (call-next-method)
  (format stream "</~a>" (html-tag tag)))

(defparameter *tags* (make-hash-table :test 'equalp))

(defmacro def-simple-tag (name markup-tag html-tag &optional superclasses extra-slots)
  `(progn
     (defclass ,name ,(or superclasses '(simple-tag))
       ((html-tag :initform ,html-tag)
        ,@extra-slots))
     (setf (gethash ,markup-tag *tags*) ',name)))

(def-simple-tag tag-bold "b" "strong")

(def-simple-tag tag-emph "i" "em")

(def-simple-tag tag-url "url" "a")

(defmethod html-attrs ((tag tag-url))
  (with-slots (args content) tag
    (let ((url (cond (args (car args))
                     ((stringp (car content)) (car content))
                     (t "#"))))
      (format nil "href=\"~a\"" (escape-string url)))))

(defun make-tag (markup-tag args content)
  (let ((class (gethash markup-tag *tags*)))
    (when class
      (make-instance class :args args :content content))))
 
(defun format-node (node stream)
  (cond
    ((stringp node) 
     (replace-line-breaks (escape-string node) stream))
    (t (destructuring-bind ((name &rest args) &rest content) node
         (let ((tag (make-tag name args content)))
           (if tag
               (format-tag tag stream)
               (progn
                 (format stream "[~a]" (escape-string name))
                 (format-tree content stream))))))))
               
(defun format-tree (tree stream)
  (dolist (token tree) (format-node token stream)))

(defun format-post (text &optional (stream *standard-output*))
  (format-tree (parse-post text) stream))

(defun format-post-to-string (text)
  (with-output-to-string (s)
    (format-post text s)))
