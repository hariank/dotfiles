;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Hariank Muthakana"
      user-mail-address "hariank3k@gmail.com")

;; for gui
(setq doom-font "Inconsolata-14"
      doom-variable-pitch-font "Inconsolata-14")
(setq doom-theme 'doom-vibrant)
(setq display-line-numbers-type t)

;; default project directories
(projectile-add-known-project "~/dotfiles")
(projectile-add-known-project (getenv "SOURCE"))

(global-visual-line-mode t) ;; visual line wrap
(setq scroll-margin 8)

;; when browsing wrapped lines scroll visually instead of jumping
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

;; general autocomplete
;; (after! company
;;   (setq company-idle-delay 0.1
;;         company-minimum-prefix-length 2)
;;   (setq company-global-modes '(not org-mode))
;; (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.

;; tags
(setq tags-revert-without-query 1)

;; flycheck
(setq flycheck-global-modes nil)
;; (after! flycheck
;;   (setq-default flycheck-checker 'python-pylint))

;; go mode settings
(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
)
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; python mode settings
(defun my-python-mode-hook ()
  (setq indent-tabs-mode nil tab-width 2)
  ;; (idle-highlight-mode t)
)
(add-hook 'python-mode-hook 'my-python-mode-hook)

;; (global-set-key (kbd "<backtab>") 'hs-toggle-hiding)
;; (define-key python-mode-map (kbd "<backtab>") nil)

(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; doom prompts speeedup and cleanup
(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))

