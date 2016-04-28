;; This file holds a bunch of elisp functions I wrote or stole.

(require 'dash)
(require 's)

;; Convenience function for adding multiple bindings to the same keymap.
;; Example: (add-bindings keymap (list (list (kbd "M-a") 'someFunction)))
(defun add-bindings (keymap key-command-pairs)
  (if key-command-pairs
    (progn
      (let ((pair (car key-command-pairs)))
	(let ((binding (car pair))
	      (command (cadr pair)))
	  (define-key keymap binding command)))
      (add-bindings keymap (cdr key-command-pairs)))
    "Bindings added"))

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))


(defun today ()
  "Insert string for today's date nicely formatted in American style,
e.g. Monday, September 17, 2000."
  (interactive)
  (insert (format-time-string "%A, %B %e, %Y")))

(defun pk/terse-date-entry ()
  "Insert string for today's date formatted tersely in American
  style, e.g. 9/17/2000."
  (interactive)
  (insert (format-time-string "%b %d, %Y"))
  (insert "\n    "))

(defun pk/add-braindump-entry ()
  "Insert a new entry in my bare-bones braindump file. If this is the
first entry of the day, add the date as well.

I would like to have one blank line before each entry, and two
blank lines before the date. Making that happen is a little
tricky; right now it only works if there are no trailing blank
lines (if there are it'll keep one of them in addition to the
inserted blank lines). Making delete-trailing-whitespace actually
delete that last blank line would fix this."
  (interactive)
  (delete-trailing-whitespace)
  (let ((date-string (format-time-string "%b %d, %Y")))
    (beginning-of-buffer)
    (if (not (search-forward date-string nil t))
	(progn (end-of-buffer)
	       (insert "\n\n\n")
	       (insert date-string)))
    (end-of-buffer)
    (insert "\n\n")))


;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))


;; Function I bound to C-q, to easily quit pop-up help windows
(defun quit-next-help-window ()
  (interactive)
  (other-window 1)
  (if (eq major-mode 'help-mode)
      (quit-window)
    (other-window -1)))


(defun move-line-up ()
  "Move the current line up"
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))
(defun move-line-down ()
  "Move the current line down"
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))


(defun set-margins (left right)
  "Set the left and right margins of the current window"
  (set-window-margins (get-buffer-window) left right))

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line. Move point to
   the first non-whitespace character on this line. If point is
   already there, move to the beginning of the line. Effectively
   toggle between the first non-whitespace character and the beginning
   of the line.

   If ARG is not nil or 1, move forward ARG - 1 lines first. If point
   reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))


(defvar pk/punctuation-regexp "[][(){}<>#^_.?!,;:\\/|~=-]")

(defun pk/kill-through-previous-punctuation ()
  (interactive)
  (let ((end (point))
	(begin (search-backward-regexp pk/punctuation-regexp)))
    (kill-region begin end)))


;; NB turns out there's a builtin function for this, window-text-height
(defun pk/get-screen-height ()
  "Get the height of the screen in visual lines."
  (interactive)
  (let ((dummy-bufname "pk/screen-height-dummy-buffer")
	(old-buf (window-buffer)))
    (get-buffer-create dummy-bufname)
    (set-buffer dummy-bufname)
    
    ;; fill the buffer with many empty lines
    (let ((i 0))
      (while (< i 200)
	(insert "\n")
	(setq i (+ i 1))))

    ;; display dummy buffer in current window and get its height
    (set-window-buffer nil dummy-bufname)
    (beginning-of-buffer)
    (scroll-up-command)
    (let ((n (+ (line-number-at-pos) (- next-screen-context-lines 1))))
      ;; return to old buffer and window, kill dummy buffer
      (set-buffer old-buf)
      (set-window-buffer nil old-buf)
      (kill-buffer dummy-bufname)
      n)))
(defun pk/set-screen-height ()
  "Set pk/screen-height to the height of the current window in visual lines."
  (interactive)
  (setq pk/screen-height (pk/get-screen-height)))


;; Google-specific

(if (eq system-type 'gnu/linux)
    ;; set some paths involving my citc client name
    (progn
      (defun get-citc-path ()
	"Get the part of the path before google3, e.g.
        '/google/src/cloud/pkreiner/citc-name/'. Must be called
        from a buffer whose path includes this path."
	(substring buffer-file-name 0 (string-match "google3" buffer-file-name)))

      (setq northstar-common-rel-path "google3/ads/display/northstar/gae/common/")
      (setq mountie-rel-path "google3/ads/display/northstar/gae/workflows/mountie/")
      (setq northstar-rel-path "google3/ads/display/northstar/gae/")
      (defun get-northstar-common-abs-path ()
	(concat (get-citc-path) northstar-common-rel-path))
      (defun get-mountie-abs-path ()
	(concat (get-citc-path) mountie-rel-path))
      (defun get-northstar-abs-path ()
	(concat (get-citc-path) northstar-rel-path))))


;; Call after sexp to append evaluated result. For example:
;; (+ 3 4)
;; (+ 3 4) = 7
;; Nice for doing calculations in a scratch buffer. Similar to
;; print-eval-last-sexp, but with " = " instead of a newline.
(defun pk/nice-print-eval-last-sexp ()
  (interactive)
  (let ((standard-output (current-buffer))
	(result (eval-last-sexp nil)))  ;; store but don't output the result.
    (princ " = ")
    (princ result)))

(defun margins (size)
  (interactive "nEnter margin width: ")
  (setq left-margin-width size)
  (setq right-margin-width size)
  (set-window-buffer (get-buffer-window) (buffer-name)))
  
;; (defun margins (size)
;;   (interactive "nEnter margin width: ")
;;   (set-window-margins (get-buffer-window) size size))


(defun increase-margins (amount)
  (interactive)
  (let* ((current-margin (car (window-margins)))
	 (size (if current-margin
		   current-margin
		 0)))
    (margins (+ size amount))))
(defun decrease-margins (amount)
  (interactive)
  (let* ((current-margin (car (window-margins)))
	 (size (if current-margin
		   current-margin
		 0)))
    (margins (- size amount))))
(defun increment-margins ()
  (interactive)
  (increase-margins 1))
(defun decrement-margins ()
  (interactive)
  (decrease-margins 1))
(defhydra hydra-adjust-margins (global-map "M-z m")
  "adjust margins"
  ("t" increment-margins "increase")
  ("h" decrement-margins "decrease")
  ("n" nil "exit"))


(defun pk/pp-list (lst)
  (-reduce 'concat (-interpose "\n" (-map 'pp-to-string lst))))

(defun print-stack ()
  (interactive)
  (let* ((stack go-back/point-stack)
	 (index go-back/point-stack-index)
	 (upper-stack (-slice stack 0 index))
	 (lower-stack (-slice stack index))
	 (len (length go-back/point-stack)))
    (save-current-buffer
      (set-buffer (get-buffer-create "*Go-Back-Stack*"))
      (erase-buffer)
      (insert (concat "Length: " (pp-to-string len)))
      (insert "\n")
      (insert (concat "Index: " (pp-to-string index)))
      (insert "\n")
      (insert (pk/pp-list upper-stack))
      (insert "\n")
      (insert "------------------------\n")
      (insert (pk/pp-list lower-stack)))))
  

(defun refresh-buffer ()
  (interactive)
  (revert-buffer t t))


(defun pk/join-paths (path1 path2)
  (let ((stripped1 (s-chop-suffix "/" path1))
	(stripped2 (s-chop-prefix "/" path2)))
    (concat stripped1 "/" stripped2)))
  
(defun pk/desktop-change-dir ()
  (interactive)
  (save-some-buffers)
  (command-execute 'pk/desktop-change-dir-helper))
(defun pk/desktop-change-dir-helper (project-name)
  (interactive "sProject name: ")
  (let ((base-dir pk/desktop-dir))
    (desktop-change-dir (pk/join-paths base-dir project-name))))

(defun pk/new-project-desktop (project-name)
  "Save current frame, window, buffer etc. arrangement as a new
desktop, a new project, with a name given by the user."
  (interactive "sName of new project: ")
  (let* ((base-dir pk/desktop-dir)
	 (full-dir (pk/join-paths base-dir project-name)))
    (make-directory full-dir)
    (desktop-save full-dir)))



(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))
(bind-key "M-z h" 'toggle-window-split)

 (defun transpose-windows (arg)
   "Transpose the buffers shown in two windows."
   (interactive "p")
   (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
     (while (/= arg 0)
       (let ((this-win (window-buffer))
             (next-win (window-buffer (funcall selector))))
         (set-window-buffer (selected-window) next-win)
         (set-window-buffer (funcall selector) this-win)
         (select-window (funcall selector)))
       (setq arg (if (plusp arg) (1- arg) (1+ arg))))))
(bind-key "M-z m" 'transpose-windows)
