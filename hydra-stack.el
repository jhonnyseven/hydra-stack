;; -*- lexical-binding: t -*-
(require 'hydra)
(require 'cl) ;; For the assert

(defvar hydra-stack--current-body nil "Symbol of current hydra's body.")
;; (make-variable-buffer-local 'hydra-stack--current-body)
(defvar hydra-stack--stack '())

(defun hydra-stack-push (fun &optional nodup &rest funs)
  "Push `(cons fun funs)' to the call stack so that `fun' is at
the top. If `nodup' is non-nil don't duplicate the function
currently on top of the stack."
  (interactive)
  (let ((fs (nreverse (cons fun funs))))
    (if (and nodup
	     (eq (car fs) (car hydra-stack--stack)))
	(setq fs (cdr fs)))
    (dolist (f fs)
      (push f hydra-stack--stack))))

(defun hydra-stack-body-pre (body-symbol)
  "Put this in `:body-pre'. BODY-SYMBOL should be the body function
of the hydra (the one ending in /body)."
  (setq hydra-stack--current-body body-symbol))

(defun hydra-stack-post ()
  "Put this last in `:after-exit-head'. This will call top of the call stack
if the hydra is exiting."
  (setq hydra-stack--current-body nil)
  (hydra-stack--call-next))

(defun hydra-stack-current-body ()
  "Returns the body function of current hydra."
  (interactive)
  hydra-stack--current-body)

(defun hydra-stack-pop ()
  "Remove the top function from the stack."
  (pop hydra-stack--stack))

(defun hydra-stack-stack-empty-p ()
  "Is the stack empty?"
  (null hydra-stack--stack))

(defun hydra-stack-clear (&rest new-contents)
  "Empty the stack. Optionally set the new stack contents to
NEW-CONTENTS"
  (interactive)
  (setq hydra-stack--stack new-contents))

(defun hydra-stack-push-current ()
  "Push current hydra's body to the top of the stack."
  (assert hydra-stack--current-body
	  t "current-body not set, do you call `hydra-stack-body-pre' in :body-pre")
  (assert (functionp hydra-stack--current-body))
  (hydra-stack-push hydra-stack--current-body t))

(defun hydra-stack-returning (other)
  "Call `other' outside of current hydra and return back to the
original hydra. "
  (assert hydra-stack--current-body)
  (assert (functionp hydra-stack--current-body))
  (hydra-stack-push other t hydra-stack--current-body))

(defun hydra-stack-call-and-return (fun)
  "First calls `hydra-stack-push-current' and then `fun'
  interactively."
  (let ((cur hydra-stack--current-body))
    (hydra-stack-push
     (lambda ()
       (interactive)
       (if (commandp fun)
	   (call-interactively fun)
	 (funcall fun))
       (funcall cur)))))

(defun hydra-stack--call-next ()
  "Pop and call the top function on the stack if the top element
is the symbol'exit' or FORCE is non-nil."
  (when hydra-stack--stack
    (let ((fun (pop hydra-stack--stack)))
      (if (commandp fun)
	  (call-interactively fun)
	(funcall fun)))))

(defun hydra-stack-call-next ()
  "Call and pop top of the stack."
  (interactive)
  (hydra-stack--call-next))

;; Clear the stack if terminal or frame is changed

(add-hook 'server-visit-hook 'hydra-stack-clear)
(add-hook 'server-switch-hook 'hydra-stack-clear)

(defadvice handle-switch-frame (before visit-frame-clear-all activate)
  (hydra-stack-clear))

(provide 'hydra-stack)
