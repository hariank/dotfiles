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

(global-visual-line-mode t) ;; visual line wrap
(setq scroll-margin 4)

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
(after! company
  ;; (setq company-idle-delay 0.1
  ;;       company-minimum-prefix-length 2)
(add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.

;; tags
(setq tags-revert-without-query 1) ;; auto-reread tags when changed

(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
)
(add-hook 'go-mode-hook 'my-go-mode-hook)

(add-hook 'rjsx-mode-hook  'prettier-js-mode)
(add-hook 'js2-mode-hook  'prettier-js-mode)
(add-hook 'typescript-mode-hook  'prettier-js-mode)

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

(after! python
  (setq lsp-disabled-clients '(mspyls))
  (setq lsp-diagnostic-package :none)
  (setq lsp-pyls-plugins-pylint-enabled nil)
  (setq lsp-pyls-plugins-autopep8-enabled nil)
  (setq lsp-pyls-plugins-yapf-enabled nil)
  (setq lsp-pyls-plugins-pyflakes-enabled nil)
  (setq lsp-pyls-plugins-mccabe-enabled nil)
  (setq lsp-pyls-plugins-flake8-enabled nil)
  (setq lsp-pyls-plugins-pycodestyle-enabled nil)
  (setq lsp-pyls-plugins-pycodestyle-ignore t)
  (setq lsp-prefer-flymake nil)
)

(after! keychain-environment 'keychain-refresh-environment)

(after! neotree
  (setq neo-window-width 40)
  (setq doom-themes-neotree-file-icons nil)
  (setq doom-themes-neotree-enable-folder-icons nil)
)

(map! "C-h" #'evil-window-left
      "C-l" #'evil-window-right
      "C-k" #'evil-window-up
      "C-j" #'evil-window-down)

(after! solidity
  (define-key map "\C-j" nil))

;; rust
(after! rust-mode
  (setq rust-format-show-buffer nil)
  (setenv "PATH" (concat (getenv "PATH") (expand-file-name "~/.cargo/bin")))
  (setq exec-path (append exec-path `((expand-file-name "~/.cargo/bin"))))
  (add-hook 'before-save-hook
            (lambda () (when (eq major-mode 'rust-mode) (rust-format-buffer))))
)
