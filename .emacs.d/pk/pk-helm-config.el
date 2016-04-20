;; Helm Configuration
(provide 'pk-helm-config)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x u") 'helm-mini)
(global-set-key (kbd "C-x C-u") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
; rebind helm-occur over builtin regexp isearch forwards
(global-set-key (kbd "M-C-s") 'helm-occur)

;; Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
; rebind tab to run persistent action
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
 ; make TAB works in terminal
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
 ; list actions using C-z
(define-key helm-map (kbd "C-z")  'helm-select-action)
;;


(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))
;  helm buffer inside current window, not occupy whole other window
(setq helm-split-window-in-side-p           t
; move to end or beginning of source when reaching top or bottom of source.
      helm-move-to-line-cycle-in-source     t
; search for library in `require' and `declare-function' sexp.      
      helm-ff-search-library-in-sexp        t
; scroll 8 lines other window using M-<next>/M-<prior>      
      helm-scroll-amount                    8 
      helm-ff-file-name-history-use-recentf t)
; enable man searching from point
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

(helm-mode 1)


;; helm-ag

;; Make ag searches case-insensitive
(setq helm-ag-command-option "-i")

;; Note: helm-do-ag seems to make mistakes sometimes. It doesn't handle spaces in patterns correctly.


;; End Helm Configuration
