;; A file to put my key bindings in.


;; Global

(global-set-key (kbd "C-M-d") 'kill-sentence)

;; Go up and down five lines at a time with M-p, M-n
(defun pk/forward-five-lines () (interactive) (next-line 5))
(defun pk/back-five-lines () (interactive) (previous-line 5))
(define-key global-map (kbd "M-n") 'pk/forward-five-lines)
(define-key global-map (kbd "M-p") 'pk/back-five-lines)
		
;; Set C-k to kill the whole line, not just what's after the cursor
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-unset-key (kbd "C-q"))
(global-set-key (kbd "C-q") 'quit-next-help-window)
(global-unset-key (kbd "C-b"))
;; (global-set-key (kbd "C-b C-m") 'discover-my-major)
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sentence)
(global-set-key (kbd "M-s-<up>") 'move-line-up)
(global-set-key (kbd "M-s-<down>") 'move-line-down)
;; Nice commands for working with units of text. Note: add-bindings is
;; defined in useful-functions.el. These were written for working with
;; non-code. I may rebind them to work with code-meaningful units
;; later. I tried to make my own nice syntax for declaring
;; keybindings, but I didn't quite use alists the right way.
;; Someday...
;; (add-bindings
;;  (current-global-map)
;;  (list
;;   (list (kbd "C-M-f") 'forward-sentence)
;;   (list (kbd "C-M-b") 'backward-sentence)
;;   (list (kbd "C-M-<backspace>") 'backward-kill-sentence)
;;   (list (kbd "C-M-d") 'kill-sentence)
;;   (list (kbd "C-s-f") 'forward-paragraph)
;;   (list (kbd "C-s-b") 'backward-paragraph)
;;   (list (kbd "C-s-<backspace>") 'backward-kill-paragraph)
;;   (list (kbd "C-s-d") 'kill-paragraph)
;;   ))
(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)

(global-set-key (kbd "C-h") 'backward-char)
(global-set-key (kbd "C-t") 'forward-char)
(global-set-key (kbd "C-M-h") 'backward-sentence)
(global-set-key (kbd "C-M-t") 'forward-sentence)

;; (global-set-key (kbd "C-s") 'delete-backward-char)
;; (global-set-key (kbd "C-w") 'pk/backward-kill-word-punct)
;; (global-set-key (kbd "M-d") 'pk/kill-word-punct)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "M-m") 'pk/kill-through-previous-punctuation)

(global-set-key (kbd "C-z") 'isearch-forward)
(global-set-key (kbd "C-b") 'help-command)
(bind-key "C-r" 'emacs-index-search help-map)


;; letting M-z be my personal prefix
(global-unset-key (kbd "M-z"))
(global-set-key (kbd "M-z d") 'today)
(defun pk/goto-other-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))
(bind-key "M-z b" 'pk/goto-other-buffer)
 

(global-set-key (kbd "C-M-g") 'helm-do-ag)


;; Programming

(define-key emacs-lisp-mode-map (kbd "C-x C-b") 'eval-buffer)

(define-key haskell-mode-map (kbd "C-c o") 'hoogle)
(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
(define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

(define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)

(define-key prog-mode-map (kbd "M-;") 'comment-or-uncomment-region-or-line)

;; helm-do-ag bindings for searching specific directories
(if (eq system-type 'gnu/linux)
    (progn
      (global-set-key (kbd "C-c s m") 'helm-ag-mountie)
      (global-set-key (kbd "C-c s c") 'helm-ag-northstar-common)))

;; bindings for opening and searching over files


;(global-set-key (kbd "M-f e") 'helm-cmd-t-elisp)
(global-set-key (kbd "M-f f") 'helm-find-files)
(global-set-key (kbd "M-f M-f") 'helm-find-files)
(defun pk/go-to-dot-emacs () (interactive) (find-file "~/.emacs"))
(global-set-key (kbd "M-f d") 'pk/go-to-dot-emacs)
(defun pk/go-to-notes () (interactive) (find-file "~/notes/notes.org"))
(global-set-key (kbd "M-f n") 'pk/go-to-notes)
(defun pk/go-to-personal-notes () (interactive)
       (find-file "~/notes/personal.org"))
(global-set-key (kbd "M-f p") 'pk/go-to-personal-notes)
(defun pk/go-to-jobs-notes () (interactive)
       (find-file "~/notes/jobs.org"))
(bind-key "M-f j" 'pk/go-to-jobs-notes)
;; (defun pk/go-to-braindump () (interactive) (find-file "~/notes/braindump"))
;; (global-set-key (kbd "M-f c") 'pk/go-to-braindump)
;; (global-set-key (kbd "M-z e") 'pk/add-braindump-entry)
;; (global-set-key (kbd "M-z M-e") 'pk/add-braindump-entry)

;; org-mode

(define-key global-map (kbd "C-c c") 'org-capture)
(define-key org-mode-map (kbd "M-e") 'end-of-line)
