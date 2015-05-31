(require 'hydra)
(require 'hydra-stack)

;; From aboabo
(defmacro hydra-with (start expected &rest body)
  `(let ((temp-buffer (generate-new-buffer " *temp*")))
     (save-window-excursion
       (let ((res (unwind-protect
		      (progn
			(switch-to-buffer temp-buffer)
			(transient-mark-mode 1)
			(insert ,start)
			(goto-char (point-min))
			(when (search-forward "@" nil t)
			  (backward-delete-char 1)
			  (set-mark (point)))
			(goto-char (point-max))
			(search-backward "|")
			(delete-char 1)
			(setq current-prefix-arg)
			,@body
			(insert "|")
			(when (region-active-p)
			  (exchange-point-and-mark)
			  (insert "~"))
			(buffer-substring-no-properties
			 (point-min)
			 (point-max)))
		    (and (buffer-name temp-buffer)
			 (kill-buffer temp-buffer))
		    'error)))
	 (should (equal ,expected res))))))

(defhydra hydra-a (
		   :color amaranth
		   :body-pre (progn
			       (hydra-stack-body-pre 'hydra-a/body)
			       (insert "(<a) "))
		   :after-exit-head (progn
				      (insert "(a>) ")
				      (hydra-stack-post)))
  "a"
  ("l" (hydra-stack-push 'hydra-b/body t
			 (lambda ()
			   (message "returned to a")
			   (hydra-a/body)))
   "visit hydra-b" :exit t)
  ("i" (insert "A"))
  ("u" undo)
  ("j" next-line)
  ("h" nil "exit" :exit t))

(defhydra hydra-b (
		   :body-pre (progn
			       (hydra-stack-body-pre 'hydra-b/body)
			       (insert "(<b) "))
		   :after-exit-head (progn
			   (insert "(b>) ")
			   (hydra-stack-post)))
  "b"
  ("l" (hydra-stack-returning 'hydra-c/body)
   "visit hydra-c" :exit t)
  ("i" (message "nonexiting"))
  ("h" nil "exit" :exit t))

(defhydra hydra-c (
		   :body-pre (progn
			       (hydra-stack-body-pre 'hydra-c/body)
			       (insert "(<c) "))
		   :after-exit-head (progn
			   (insert "(c>) ")
			   (hydra-stack-post)))
  "c"
  ("h" nil "exit" :exit t))

(global-set-key (kbd "r") 'hydra-a/body)

(ert-deftest testen0 ()
  (hydra-with "|" "(<a) (a>) |"
	      (execute-kbd-macro
	       (kbd "rh"))))

(ert-deftest testen2 ()
  (hydra-with
   "|" "(<a) AA(a>) |"
   (execute-kbd-macro
    (kbd "riih"))))

(ert-deftest testen3 ()
  (hydra-with "|" "(<a) (a>) (<b) (b>) (<c) (c>) (<b) (b>) (<a) (a>) h|"
   (execute-kbd-macro
    (kbd "rllhhhh"))))
