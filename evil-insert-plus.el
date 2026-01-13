;;; evil-insert-plus.el --- Use insert and append as operators -*- lexical-binding: t; -*-

;; Author: Yad Tahir <yad at ieee.org>
;; URL: https://github.com/yad-tahir/evil-insert-plus
;; Package-Requires: ((emacs "24.4") (evil "1.14.0"))
;; Version: 0.4
;; Keywords: evil, vim, editing

;;; Commentary:
;; This package allows you to use insertion commands with motions and text
;; objects.

;;; Code:

(require 'evil)

(defun evil--insert-plus-target (beg end &optional type is-append)
  "Return the point where insertion should occur for BEG, END, and TYPE.
IS-APPEND determines if the operation is an append or insert."
  (let ((p-before (point))
		(inhibit-modification-hooks t)
		;; Suppress macro and undos
		(defining-kbd-macro nil)
		(executing-kbd-macro t)
		(buffer-undo-list nil))
	(catch 'evil-insert-plus-quit
	  (save-excursion
		(atomic-change-group
		  (let ((size-before (buffer-size)))
			;; Perform deletion into the black hole register
			(evil-delete beg end type ?_)
			(let* ((displacement (- size-before (buffer-size)))
				   (result (if (= p-before (point)) ;; Forward motion
							   (if is-append (+ beg displacement) beg)
							 ;; Backward motion
							 (if is-append end (point)))))
			  ;; Force a rollback of the deletion
			  (throw 'evil-insert-plus-quit result))))))))

(defun evil--insert-plus-vcount ()
  "Calculate line count for visual line/block insertions."
  (when (and (evil-visual-state-p)
			 (memq (evil-visual-type) '(line block)))
	(save-excursion
	  (let ((m (mark)))
		(evil-visual-rotate 'upper-left)
		(prog1 (count-lines evil-visual-beginning evil-visual-end)
		  (set-mark m))))))

;;;###autoload
(evil-define-operator evil-insert-plus (beg end &optional type count)
  "Perform `evil-insert' targeting the range defined by a motion."
  :move-point nil
  (interactive "<R><c>") ; <R> for range and type, <c> for count
  (let ((vcount (evil--insert-plus-vcount)))
	(cond
	 ((eq type 'line)
	  ;; visual-goto-line motions - e.g. `evil-goto-first-line'
	  (unless (or (evil-visual-state-p)
				  (eq evil-this-motion 'evil-line-or-visual-line))
		(goto-char beg))
	  (evil-insert-line count vcount))
	 ((eq type 'block)
	  (goto-char beg)
	  (evil-insert count vcount))
	 (t
	  (goto-char (evil--insert-plus-target beg end type nil))
	  (evil-insert 1)))))

;;;###autoload
(evil-define-operator evil-append-plus (beg end &optional type count)
  "Perform `evil-append' targeting the range defined by a motion."
  :move-point nil
  (interactive "<R><c>") ; <R> for range and type, <c> for count
  (let ((vcount (evil--insert-plus-vcount)))
	(cond
	 ((eq type 'line)
	  ;; visual-goto-line motions - e.g. `evil-goto-line'
	  (unless (or (evil-visual-state-p)
				  (eq evil-this-motion 'evil-line-or-visual-line))
		(goto-char end))
	  (evil-append-line count vcount))
	 ((eq type 'block)
	  (let* ((range (evil-visual-range))
			 (beg-col (evil-column (car range)))
			 (end-col (evil-column (cadr range)))
			 (right-col (max beg-col end-col)))
		(goto-char beg)
		(move-to-column (1- right-col))
		(evil-append count vcount)))
	 (t
	  (goto-char (evil--insert-plus-target beg end type t))
	  (evil-insert 1)))))

;; VIM Quirk: Make them behave similar to evil-change specially for
;; `evil-forward-word-begin' & `evil-forward-WORD-begin' motions
(add-to-list 'evil-change-commands #'evil-insert-plus)
(add-to-list 'evil-change-commands #'evil-append-plus)

(provide 'evil-insert-plus)

;;; evil-insert-plus.el ends here
