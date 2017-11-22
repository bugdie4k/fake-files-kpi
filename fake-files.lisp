(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                        (user-homedir-pathname))))
   (when (probe-file quicklisp-init)
     (load quicklisp-init)))

(ql:quickload "alexandria")
(ql:quickload "split-sequence")
(ql:quickload "trees")

(defpackage #:fake-files
  (:nicknames #:ff)
  (:use #:cl #:alexandria)
  (:export #:main))

(in-package #:fake-files)

(defparameter *tree* nil)

(defstruct ffile
  name
  data)

(defmacro define-ff-action (name &rest lambda-list/body)
  "Defines function with the `lambda-list/body' body
and a macro with the `name' which calls this function 
with all the same arguments except that the first is 
stringified."
  (let ((macro-name (symbolicate "I" (symbol-name name))))
    `(progn
       (defun ,name ,@lambda-list/body)
       (defmacro ,macro-name (c &rest args)
         (append (list ',name
                       (cond
                         ((symbolp c) `(symbol-name ',c))
                         ((stringp c) c)
                         ((numberp c) `(write-to-string ',c))))
                 args)))))

(defun @ini ()
  (setf *tree* (trees:make-binary-tree
                :avl #'string< :test #'string= :key #'ffile-name)))

(define-ff-action @add (c &optional (data (gensym)))
  (trees:insert (make-ffile :name c :data data) *tree*))

(define-ff-action @del (c)
  (trees:delete c *tree*))

(define-ff-action @get (c)
  (if-let ((res (trees:find c *tree*)))
    (values (ffile-data res) t)
    (values nil nil)))

(defun pprint-tree (tree &optional (stream *standard-output*))
  (labels ((recursive-print (node level char)
             (trees::indent-to-level level stream)
             (write-char char stream)
             (write-char #\Space stream)
             ;; (prin1 node stream)
             (format stream "~A : ~A"
                     (ffile-name (trees::datum node))
                     (ffile-data (trees::datum node)))
             (terpri stream)
             (unless (null (trees::left node))
               (recursive-print (trees::left node) (1+ level) #\l))
             (unless (null (trees::right node))
               (recursive-print (trees::right node) (1+ level) #\r))))
    (if (null (trees::root tree))
        (format stream "empty tree~%")
        (recursive-print (trees::root tree) 0 #\R))
    (values)))

(defun @show ()
  (pprint-tree *tree*))

(defun no-args-command? (cmd)
  (or (eq cmd 'ini) (eq cmd 'show)))

(defun help ()
  (format t "COMMANDS~%")
  (format t "  show                - show tree structure~%")
  (format t "  add <name> [<data>] - add file with data in it~%")
  (format t "  get <name>          - get data from file~%")
  (format t "  del <name>          - delete file~%")
  (format t "  quit                - quit~%"))

(defun menu ()
  (loop
    (format t "%-> ")
    (let ((cmds (split-sequence:split-sequence #\space (read-line))))
      (switch ((car cmds) :test string-equal)
        ("show" (@show))
        ("help" (help))
        ("add"  (let ((data (read-from-string (or (third cmds) ":empty"))))
                  (@add (second cmds) data)
                  (format t "added file ~A~%" (second cmds))))
        ("get"  (multiple-value-bind (data status) (@get (second cmds))
                    (if status
                        (format t "contents of ~A~%~A~%" (second cmds) data)
                        (format t "no such file ~A" (second cmds)))))
        ("del"  (@del (second cmds)))
        ("quit" (return))
        ("q" (return))
        (t (format t "~A is not a command~%" (car cmds)))))))

(defun main ()
  (@ini)
  (menu))