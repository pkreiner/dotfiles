(provide 'pk-org-functions)

(defun pk/org-beginning-of-entry ()
  "Move point back to beginning of entry and return its position."
  (interactive)
  ;; go to the beginning of the stars of the entry. note:
  ;; back-to-indentation does not respect visual lines, so this works
  ;; even on multi-line entries.
  (back-to-indentation)
  ;; go to the end of the stars
  (search-forward-regexp "\**")
  ;; go past the initial space
  (forward-char)
  (point))

(defun pk/org-clear-entry ()
  "Kill the contents of an entry but leave the stars."
  (interactive)
  (let ((begin (pk/org-beginning-of-entry)))
    (end-of-line)
    (kill-region begin (point))))

(defun pk/org-kill-backward-sentence-or-clear-entry ()
  "Kill backward to the beginning of the entry or the beginning
of the sentence, whichever is closest."
  (interactive)
  (let* ((start (point))
	 (sentence-begin (progn (backward-sentence) (point)))
	 (entry-begin (progn (goto-char start) (pk/org-beginning-of-entry)))
	 (begin (max sentence-begin entry-begin)))
    (goto-char begin)
    (kill-region begin start)))
	


;; Never got this to work right.
;;
;; (defun pk/org-capture-refile (goto-new-entry)
;;   "Like org-capture-refile, except that, with prefix argument
;; C-u, leaves the user at the newly filed entry instead of back at
;; the original buffer."
;;   (interactive "P")
;;   (message "starting refiling")
;;   (org-capture-refile)
;;   (message "done refiling")
;;   (if goto-new-entry
;;       ;; I couldn't figure out the right way to call
;;       ;; org-capture-refile with a C-u C-u prefix arg. Using
;;       ;; org-refile this way jumps to the latest entry.
;;       (let ((current-prefix-arg '(16)))
;; 	(call-interactively 'org-refile)))
;;   (message "done with pk/org-capture-refile"))



;; Status: unfinished.

(defun pk/org-get-level ()
  "Returns the depth of the current heading."
  (let ((start (point)))
    (beginning-of-line)
    (search-forward-regexp "\**")
    (goto-char start)
    (length (match-string 0))))

;; It's kinda janky that this leaves children visible even if they
;; weren't before. But I don't know a good way to look for children if
;; they're invisible, or to store the visibility state of the
;; headline.
(defun pk/org-has-children-p ()
  "Return t if the heading at point has children, nil if not."
  (let ((start (point))
	(current-level (pk/org-get-level)))
    (show-children)
    (outline-next-heading)
    (let ((result (> (pk/org-get-level) current-level)))
      (goto-char start)
      result)))

(defun pk/org-is-final-sibling-p ()
  (let ((start (point)))
    (beginning-of-line)
    (let ((pos (point)))
      (org-forward-heading-same-level 1)
      (let ((result (eq pos (point))))
	(goto-char start)
	result))))

(defun pk/org-goto-final-sibling ()
  (while (not (pk/org-is-final-sibling-p))
    (org-forward-heading-same-level 1)))

(defun pk/org-goto-final-child ()
  (if (pk/org-has-children-p)
      (progn (show-children)
	     (outline-next-heading)
	     (pk/org-goto-final-sibling))))

(defun pk/org-new-child (arg)
  "Add new child entry at top of children (or at bottom of
children, with prefix arg)."
  (interactive "P")

  ;; Necessary to avoid the case where we're after the ellipsis of a
  ;; folded heading and hence technically at the end of that heading's
  ;; subtree.org-beginning-of-line takes us to before the ellipsis,
  ;; and beginning-of-line takes us back to the beginning of the
  ;; entry, just to be sure.
  (org-beginning-of-line)
  (beginning-of-line)
  
  (show-children)
  (if (or arg (not (pk/org-has-children-p)))
      (progn (end-of-line)
	     (org-insert-heading)
	     (org-demote))
    (progn (pk/org-goto-final-child)
	   (end-of-line)  ;; this ensures it works on multiline headings
	   (org-end-of-line)  ;; and this goes past any hidden sub-headings
	   (org-insert-heading-respect-content))))

(defun pk/org-show-siblings ()
  (interactive)
  (let ((start (point)))
    (org-up-heading-safe)
    (show-children)
    (goto-char start)))

;; Copied from a stackoverflow answer
(defun pk/refile (file headline)
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile nil nil (list headline file nil pos))))
