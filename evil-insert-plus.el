;;; evil-insert-plus.el --- Use insert and append as operators -*- lexical-binding: t; -*-

;; Author: Yad Tahir <yad at ieee.org>
;; URL: https://github.com/yad-tahir/evil-insert-plus
;; Package-Requires: ((emacs "24.4") (evil "1.14.0"))
;; Version: 0.3
;; Keywords: evil, vim, editing

;;; Commentary:
;; This package allows you to use insertion commands with motions and text
;; objects.

;;; Code:

(require 'evil)

(evil-define-operator evil-insert-plus (beg end &optional type count)
  "Perform `evil-insert' with a motion."
  (interactive "<R><c>") ; <R> for range and type, <c> for count
  (let ((vcount (and (evil-visual-state-p)
					 (memq (evil-visual-type) '(line block))
					 (save-excursion
					   (let ((m (mark)))
						 ;; Go to upper-left corner temporarily so
						 ;; `count-lines' yields accurate results
						 (evil-visual-rotate 'upper-left)
						 (prog1 (count-lines evil-visual-beginning evil-visual-end)
						   (set-mark m)))))))
	(ignore end)
	(cond
	 ((eq type 'line)
	  (evil-insert-line count vcount))
	 (t
	  (goto-char beg)
	  (evil-insert count vcount)))))

(evil-define-operator evil-append-plus (beg end &optional type count)
  "Perform `evil-append' with a motion."
  (interactive "<R><c>") ; <R> for range and type, <c> for count
  (let ((vcount (and (evil-visual-state-p)
					 (memq (evil-visual-type) '(line block))
					 (save-excursion
					   (let ((m (mark)))
						 ;; Go to upper-left corner temporarily so
						 ;; `count-lines' yields accurate results
						 (evil-visual-rotate 'upper-left)
						 (prog1 (count-lines evil-visual-beginning evil-visual-end)
						   (set-mark m)))))))
	(cond
	 ((eq type 'line)
	  (evil-append-line count vcount))
	 ((eq type 'block)
	  (let* ((range (evil-visual-range))
			 (beg-col (evil-column (car range)))
			 (end-col (evil-column (cadr range)))
			 (right-col (max beg-col end-col)))
		(goto-char beg)
		(move-to-column (1- right-col))
		(evil-append count vcount)))
	 ((eq type 'exclusive)
	  (unless (memq evil-this-motion '(evil-forward-char
									   evil-forward-chars))
		(setq end (if (eolp) end (1- end))))
	  (goto-char end)
	  (evil-insert count vcount))
	 (t
	  (goto-char end)
	  (evil-insert count vcount)))))

(provide 'evil-insert-plus)

;;; evil-insert-plus.el ends here
