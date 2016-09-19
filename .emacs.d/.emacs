;; Paul Kreiner's .emacs file.

(require 'package)
(setq package-enable-at-startup 'nil)
;; (package-refresh-contents)
;; Note: not including marmalade because it sucks.
(setq package-archives
     '(("elpa" . "https://elpa.gnu.org/packages/")
      ("melpa" . "https://melpa.milkbox.net/packages/")))
(package-initialize)

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t)
(require 'use-package)

;; (eval-when-compile
  ;; (require 'use-package))
;; (require 'bind-key)

;; Mac-specific settings
;; (if (eq system-type 'darwin)
;;    (progn
;;      (setq mac-option-modifier 'super)
;;      (setq mac-command-modifier 'meta)
;;      ;; Set the mouse-wheel (trackpad) to scroll about two lines per event
;;      (setq mouse-wheel-scroll-amount '(0.02))))

;; Linux-specific settings
;; (if (eq system-type 'gnu/linux)
    ;; (progn
;      (require 'google)
;     (require 'google-pyformat)
;     (set-default-font "inconsolata 13")
;; ))

(set-default-font "inconsolata 12")

;; My Variables

(defconst notes-directory "/home/paul/notes/")
(defconst notes-file
  (concat (file-name-as-directory notes-directory) "notes.org"))
(defconst personal-notes-file
  (concat (file-name-as-directory notes-directory) "personal.org"))
(defconst pk/desktop-dir "~/.emacs.d/desktops/")
(defconst my-elisp-repos "~/my-elisp-repos/")

;; Loads and Imports

;; Recursively add entire ~/.emacs.d directory to load path
(let ((default-directory "~/.emacs.d"))
  (normal-top-level-add-subdirs-to-load-path))

;; Add all my elisp repos to the load path
(let ((default-directory my-elisp-repos))
  (normal-top-level-add-subdirs-to-load-path))

;; Directory for elisp I've written
(setq personal-path "~/.emacs.d/pk/")
(add-to-list 'load-path personal-path)

;; Add haskell libraries to PATH and the exec-path.
(let ((haskell-bin-path "~/Library/Haskell/bin"))
  (setenv "PATH" (concat haskell-bin-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path haskell-bin-path))

;; Unsetting keys that'll be used as prefixes later.
;; M-f is for going to files,
;; M-z is my miscellaneous personal prefix,
;; C-M-c is now the visual bookmarks (bm) prefix
(global-unset-key (kbd "M-f"))
(global-unset-key (kbd "M-z"))
(global-unset-key (kbd "C-M-c"))

(key-chord-mode 1)
(key-chord-define-global "qj" 'pk/goto-other-buffer)


(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "M-d") 'kill-word)
(global-set-key (kbd "M-h") 'backward-word)
(global-set-key (kbd "M-t") 'forward-word)

(use-package go-back
  :config
  (bind-key "M-r" 'go-back/pop-point-stack global-map))

(use-package pk-word-punct
  ;; :bind (("M-h" . pk/backward-word-punct)
  ;; 	 ("M-t" . pk/forward-word-punct)
  ;; 	 ("C-w" . backward-kill-word)
  ;; 	 ("M-d" . pk/kill-word-punct))
  :config
  (key-chord-define-global "qw" 'pk/backward-kill-word-punct)
  (key-chord-define-global "qb" 'pk/backward-word-punct)
  (key-chord-define-global "qc" 'pk/forward-word-punct))

(use-package elm-mode
  :ensure t
  :commands elm-mode)
(use-package literary-mode
  :commands literary-mode)
(use-package window-number
  :config
  (window-number-mode)
  (window-number-meta-mode))
;; (use-package color-theme)
;; ;; (color-theme-initialize)
;; ;; (load-theme 'zenburn t)
;; (use-package color-theme-solarized
;;   :config (color-theme-solarized))
;; ;; (require 'color-theme-solarized)
;; ;; (color-theme-solarized)


;; (use-package test-pkg
;;   :bind (("M-z a" . pk-test)))


(use-package org
  :config
  (require 'org-capture)
  (setq org-startup-indented t)
  (setq org-default-notes-file
	(concat org-directory "/notes.org"))
  (setq org-refile-allow-creating-parent-nodes t)
  (setq org-refile-use-outline-path t)
  ;; I don't use completion at the moment, so completing refile
  ;; targets in steps doesn't make sense. Completing the whole path at
  ;; once (with helm) seems all right.
  (setq org-outline-path-complete-in-steps nil)
  ;; Let (basically) all headings be refile targets, not just
  ;; top-level ones.
  (setq org-refile-targets '((nil . (:maxlevel . 10))))
  (setq org-tags-column 0)
  (setq org-log-done 'time)
  (setq org-capture-templates
        '(("n" "Basic Note" entry
	   (file+headline "~/notes/notes.org" "Thought Dump")
           "* %?")))
  (setq org-agenda-files '("~/notes/notes.org"))
  (setq org-ellipsis "…")
  (bind-key "C-M-h" 'org-backward-sentence org-mode-map)
  (bind-key "C-M-t" 'org-forward-sentence org-mode-map)
  (bind-key "C-k" 'org-cut-special org-mode-map)
  (bind-key "M-h" 'pk/backward-word-punct org-mode-map)
  (bind-key "C-c C-r" 'org-refile org-mode-map)
  (bind-key "C-c C-r" 'org-capture-refile org-capture-mode-map)
  (bind-key "C-c a m" 'org-tags-view)
  (bind-key "M-z c e"
	    (lambda () (interactive) (pk/refile notes-file "Emacs Done")))
  (bind-key "M-z c d"
	    (lambda () (interactive) (pk/refile notes-file "General Done")))
  (bind-key "M-f t" (lambda () (interactive)
		      (find-file notes-file)
		      (goto-char (org-find-exact-headline-in-buffer "Thought Dump"))))
  (key-chord-define-global ",."
                           (lambda () (interactive) (org-capture nil "n")))
  (use-package pk-org-functions
    :config
    (bind-key "M-a" 'pk/org-beginning-of-entry org-mode-map)
    (bind-key "M-e" 'pk/org-end-of-entry org-mode-map)
    ;; (bind-key "C-x k" 'pk/org-clear-entry org-mode-map)
    (bind-key "C-M-<return>" 'pk/org-new-child org-mode-map)
    (bind-key "C-c M-r" 'pk/org-show-siblings org-mode-map)
    (bind-key "C-M-<backspace>" 'pk/org-kill-backward-sentence-or-clear-entry)
    (bind-key "C-c C-M-r" 'pk/org-goto-final-child)))

;; (req-package pk-org-functions
;;   :require org
;;   :commands (pk/org-beginning-of-entry
;; 	     pk/org-clear-entry)
;;   :config

;; (req-package-finish)
;; (req-package--log-open-log)

;; (use-package org
;;   :config
;;   (setq org-startup-indented t)
;;   (setq org-default-notes-file
;; 	(concat org-directory "/notes.org"))
;;   (bind-key "C-M-h" 'org-backward-sentence org-mode-map)
;;   (bind-key "C-M-t" 'org-forward-sentence org-mode-map)
;;   (bind-key "C-k" 'org-cut-special org-mode-map)
;;   (bind-key "M-h" 'pk/backward-word-punct org-mode-map)
;;   (use-package pk-org-functions
;;     :config
;;     (bind-key "M-a" 'pk/org-beginning-of-entry org-mode-map)
;;     (bind-key "C-x k" 'pk/org-clear-entry org-mode-map)))

;; (use-package helm
;;   :config
;;   (use-package helm-config)
;;   (use-package pk-helm-config
;;     :bind ("M-t e" . helm-cmd-t-elisp)))
  
(require 'helm)
(require 'helm-config)
;; (require 'helm-cmd-t)			
;; (require 'pk-helm-config)
(require 'pk-helm-config)
(use-package pk-helm-functions
  :bind ("C-M-s" . pk/helm-occur-or-org-buffer-headings))
(use-package helm-cmd-t
  :bind ("M-f e" . helm-cmd-t-elisp)
  :config (use-package pk-helm-cmd-t-config))

(setq helm-swoop-split-direction 'split-window-horizontally)

(global-anzu-mode 1)


(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))
	 
(use-package bm
  :ensure t
  :bind (("C-M-c b" . bm-toggle)
	 ("C-M-c n" . bm-next)
	 ("C-M-c p" . bm-previous))
  :init (setq bm-restore-repository-on-load t)
  :config (use-package bm-config))

(use-package tex
  :ensure auctex)

(require 'ein)

(require 'haskell-mode)
(require 'haskell-interactive-mode)
(require 'haskell-process)

(defun pk/turn-on-haskell-interactive-mode-with-kbds ()
  (interactive-haskell-mode 1)
  (define-key interactive-haskell-mode-map (kbd "M-n") 'pk/forward-five-lines)
  (define-key interactive-haskell-mode-map (kbd "M-p") 'pk/back-five-lines)
  (define-key interactive-haskell-mode-map (kbd "M-t") 'forward-word)
  (define-key interactive-haskell-mode-map (kbd "C-M-n") 'ghc-goto-next-error)
  (define-key interactive-haskell-mode-map (kbd "C-M-p") 'ghc-goto-prev-error)
  (define-key interactive-haskell-mode-map (kbd "C-M-t") 'ghc-insert-template-or-signature)
  (define-key interactive-haskell-mode-map (kbd "C-M-.") 'xref-pop-marker-stack))
(add-hook 'haskell-mode-hook 'pk/turn-on-haskell-interactive-mode-with-kbds)
;; (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;; (add-hook 'haskell-mode-hook 'set-interactive-haskell-mode-keybindings)

;; ghc-mod is very useful but also very janky. It rudely overwrites my
;; keybindings the first time ghc-init runs. This is a hack to fix
;; that by writing them again after ghc-init.

;; (autoload 'ghc-init "ghc" nil t)
;; (autoload 'ghc-debug "ghc" nil t)
;; (add-hook 'haskell-mode-hook 'ghc-init-and-keybindings)



;; Load some useful functions
(load (concat personal-path "useful-functions.el"))
;; After importing all packages, set keybindings
(load (concat personal-path "keybindings.el"))
(bind-key "M-z r" 'refresh-buffer)
(unbind-key (kbd "C-x C-r"))
(bind-key "C-x C-r SPC" 'point-to-register)
(bind-key "C-x C-r j" 'jump-to-register)

;; Hydra for adjusting the text body width
;; (defun 

;; (defhydra "adjust-margins" (global-map "M-z m")
  ;; "adjust margins"
  ;; ("
  


;; Miscellaneous Settings

;; make C-c work like c, after the help prefix
(define-key help-map (kbd "C-c") 'describe-key-briefly)

(define-key help-map (kbd "C-l") 'elisp-index-search)


(set-scroll-bar-mode nil)
(setq ring-bell-function 'ignore)
(tool-bar-mode 0)
;; Save desktops, without asking
(desktop-save-mode)
(setq desktop-save t)
;; Auto-save every 20 characters
(setq auto-save-interval 20)
;; store all backup and autosave files in ~/emacs-tmp
(setq temporary-file-directory "~/emacs-tmp/")
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;; Sentences are separated by only one space after the period
(setq sentence-end-double-space nil)
 ;; show line numbers in prog-mode. Note that linum-mode is dangerous
 ;; in that it makes it impossible to set left margins
(add-hook 'prog-mode-hook 'linum-mode)
;; Enable soft word-wrap in text-mode
(add-hook 'text-mode-hook 'visual-line-mode)

(setq initial-scratch-message nil)
(menu-bar-mode -1)


;; Authentication
(setq pk/authentication-file "~/.emacs.d/.emacs-authentication.el")
(add-to-list 'load-path "~")
(load-file pk/authentication-file)


;; Flycheck
(require 'flycheck)
(setq flycheck-check-syntax-automatically '(mode-enabled save))
;; (setq flycheck-idle-change-delay 1.0)


;; Programming

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)

(setq eldoc-idle-delay 0)


;; Haskell

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; (remove-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

(setq haskell-hoogle-command "~/Library/Haskell/bin/hoogle")
;; (setq haskell-tags-on-save t) ;; hasktags doesn't work for me

(setq haskell-doc-idle-delay 0)
(add-to-list 'completion-ignored-extensions ".hi")

;; Abbreviations for inserting special characters.
(define-abbrev-table 'global-abbrev-table
  '(("8sigma"		 "𝚺")
    ("8delta"		 "δ")
    ("8intersect"	 "∩")
    ("8contains"	 "∋")
    ("8member"		 "∈")
    ("8nin"		 "∉")
    ("8inf"		 "∞")
    ("8ep"		 "ε")
    ("8neq"		 "≠")
    ("8superset"	 "⊇")
    ("8subset"		 "⊆")
    ("8psuperset"	 "⊃")
    ("8psubset"		 "⊂")
    ("8eq"               "≡")
    ("8neqv"             "≢")
    ("8forall"           "∀")
    ("8exists"           "∃")
    ("8cappi"            "∏")
    ("8pi"               "π")
    ("8alpha"            "α")
    ("8beta"             "β")
    )
  )
(abbrev-mode 1)


;; Customizations for the Solarized theme

;; Don't change size of org-mode headlines (but keep other size-changes)
(setq solarized-scale-org-headlines nil)
;; Don't change the font for some headings and titles
(setq solarized-use-variable-pitch nil)

;; ;; Avoid all font-size changes
;; (setq solarized-height-minus-1 1)
;; (setq solarized-height-plus-1 1)
;; (setq solarized-height-plus-2 1)
;; (setq solarized-height-plus-3 1)
;; (setq solarized-height-plus-4 1)


;; Add shortcuts for changing themes easily
(bind-key "M-z t" 'customize-themes)
;; And changing fonts easily
(bind-key "M-z f SPC" 'set-frame-font)
(defconst garamond-font-string "-unknown-GaramondNo8-normal-normal-normal-*-16-*-*-*-*-0-iso10646-1")
(defconst monospace-font-string "-unknown-Inconsolata-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1")
(bind-key "M-z f g" (lambda () (interactive) (set-frame-font garamond-font-string)))
(bind-key "M-z f m" (lambda () (interactive) (set-frame-font monospace-font-string)))

(bind-key "M-N" (lambda () (interactive) (scroll-up-line 5)))
(bind-key "M-P" (lambda () (interactive) (scroll-down-line 5)))


;; (setq org-image-actual-width t)


(setq ag-highlight-search t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "99953b61ecd4c3e414a177934e888ce9ee12782bbaf2125ec2385d5fd732cbc2" "708df3cbb25425ccbf077a6e6f014dc3588faba968c90b74097d11177b711ad1" "868f73b5cf78e72ca2402e1d48675e49cc9a9619c5544af7bf216515d22b58e7" "404a8e7f198ef3a5babdf122c7905abc61a8cd04eb2a1ce7d6faec5550b02a90" "19285cf35ec4b8104901d41a88d4603c97c52c594c4d2521ffcf17be15f97096" "38ba6a938d67a452aeb1dada9d7cdeca4d9f18114e9fc8ed2b972573138d4664" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "cab317d0125d7aab145bc7ee03a1e16804d5abdfa2aa8738198ac30dc5f7b569" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "74278d14b7d5cf691c4d846a4bbf6e62d32104986f104c1e61f718f9669ec04b" "f81933744f47a010213537575f84085af3937b27748b4f5c9249c5e100856fc5" "613a7c50dbea57860eae686d580f83867582ffdadd63f0f3ebe6a85455ab7706" "71ecffba18621354a1be303687f33b84788e13f40141580fa81e7840752d31bf" "b7b2cd8c45e18e28a14145573e84320795f5385895132a646ff779a141bbda7e" "3b24f986084001ae46aa29ca791d2bc7f005c5c939646d2b800143526ab4d323" "fe1682ca8f7a255cf295e76b0361438a21bb657d8846a05d9904872aa2fb86f2" "4e262566c3d57706c70e403d440146a5440de056dfaeb3062f004da1711d83fc" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "cbef37d6304f12fb789f5d80c2b75ea01465e41073c30341dc84c6c0d1eb611d" default)))
 '(desktop-path (quote ("~/.emacs.d/desktops/" "~")))
 '(fci-rule-color "#073642")
 '(frame-background-mode (quote light))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")))
 '(inhibit-startup-screen t)
 '(latex-run-command "pdflatex")
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(paradox-automatically-star t)
 '(paradox-github-token t t)
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(python-indent-offset 2)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(text-scale-mode-step 1.05)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(bm-face ((t (:background "dark red" :foreground "White")))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Use the solarized light theme.

;; This needs to be done after 'custom-safe-themes' are set in the
;; customization block above.
(load-theme 'solarized-light)


