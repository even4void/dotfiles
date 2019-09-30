
;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(load "~/.lisprc")

(setf *quit-on-eof* t)
(setf *load-preserves-optimization-settings* t)

(setf ccl:*default-file-character-encoding* :utf-8)

(defun my-lisp-prompt-format (stream level)
  (if (zerop level)
    (format stream "~%~A> " (package-name *package*))
    (format stream "~%[~d] > " level)))

(setf ccl:*listener-prompt-format* #'my-lisp-prompt-format)

