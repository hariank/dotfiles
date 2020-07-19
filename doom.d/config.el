;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Hariank Muthakana"
      user-mail-address "hariank3k@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;; for gui
(setq doom-font "Inconsolata-14"
      doom-variable-pitch-font "Inconsolata-14")
(setq doom-theme 'doom-vibrant)
(setq display-line-numbers-type t)
;; (global-visual-line-mode t) ;; visual line wrap


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; project directories
(projectile-add-known-project (getenv "SOURCE"))
(projectile-add-known-project "~/dotfiles")

;; org-mode and org-roam
(add-hook 'after-init-hook 'org-roam-mode)`
(add-hook 'org-roam-mode-hook 'org-roam-buffer-toggle-display)

(setq org-directory "~/Dropbox/private/roam/")
(setq org-default-notes-file "~/Dropbox/private/roam/inbox.org")
(setq org-agenda-files '("~/Dropbox/private/roam/"))

;; if slow
;; (setq org-agenda-files '("~/Dropbox/private/roam/inbox.org"
;;                          "~/Dropbox/private/roam/projects.org"
;;                          "~/Dropbox/private/roam/backlog.org"))

(setq org-agenda-custom-commands '(("ww" "Work tasks" tags-todo "+work"
                                   ((org-agenda-files '("~/Dropbox/private/roam/projects.org"))))
                                   ("ws" "Nonwork tasks" tags-todo "-work"
                                   ((org-agenda-files '("~/Dropbox/private/roam/projects.org"))))
                                   ("bw" "Backlog work tasks" tags-todo "+work"
                                   ((org-agenda-files '("~/Dropbox/private/roam/backlog.org"))))
                                   ("bs" "Backlog nonwork tasks" tags-todo "-work"
                                   ((org-agenda-files '("~/Dropbox/private/roam/backlog.org"))))
                                   ))
;; don't wrap in agenda mode
;; (add-hook 'org-agenda-mode-hook
;;       (lambda ()
;;         (make-local-variable 'visual-line-mode)
;;         (setq visual-line-mode nil)))

;; capture items to inbox list (no nesting)
(setq org-capture-templates '(("t" "log task" entry
                               (file+headline "~/Dropbox/private/roam/inbox.org" "Task Inbox")
                               "* TODO %i%?")
                              ("j" "log thought" entry
                               (file+headline "~/Dropbox/private/roam/inbox.org" "Thoughts Inbox")
                               "* %i%?")))
;; refile to a context and optionally project in active projects or backlog
(setq org-refile-targets '(("~/Dropbox/private/roam/projects.org" :maxlevel . 2)
                           ("~/Dropbox/private/roam/backlog.org" :maxlevel . 2)
                           ))
;; autosave all org buffers periodically
`(add-hook 'auto-save-hook 'org-save-all-org-buffers)`
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)"))))
;; customize how org nesting looks
(setq org-n-level-faces `1)
(custom-theme-set-faces 'user
                        `(org-level-1 ((t (:foreground "grey")))))

(setq org-roam-directory "~/Dropbox/private/roam/")
(setq org-roam-link-title-format "%s")
;; (add-hook 'after-init-hook 'org-roam-mode)`
;; (add-hook 'org-roam-mode-hook 'org-roam-buffer-toggle-display)

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

;; tab autocompletes
;; (setq tab-always-indent 'complete)

;; tags
(setq tags-revert-without-query 1)

;; flycheck
(setq flycheck-global-modes nil)

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

;; folding leaf nodes in file - inconsistent
;; (defun hs-hide-leafs-recursive (minp maxp)
;;       "Hide blocks below point that do not contain further blocks in region (MINP MAXP)."
;;       (when (hs-find-block-beginning)
;;         (setq minp (1+ (point)))
;;         (funcall hs-forward-sexp-func 1)
;;         (setq maxp (1- (point))))
;;       (unless hs-allow-nesting
;;         (hs-discard-overlays minp maxp))
;;       (goto-char minp)
;;       (let ((leaf t))
;;         (while (progn
;;                  (forward-comment (buffer-size))
;;                  (and (< (point) maxp)
;;                       (re-search-forward hs-block-start-regexp maxp t)))
;;           (setq pos (match-beginning hs-block-start-mdata-select))
;;           (if (hs-hide-leafs-recursive minp maxp)
;;               (save-excursion
;;                 (goto-char pos)
;;                 (hs-hide-block-at-point t)))
;;           (setq leaf nil))
;;         (goto-char maxp)
;;         leaf))
;; (defun hs-hide-leafs ()
;;     "Hide all blocks in the buffer that do not contain subordinate blocks."
;;     (interactive)
;;     (hs-life-goes-on
;;       (save-excursion
;;         (message "Hiding blocks ...")
;;         (save-excursion
;;           (goto-char (point-min))
;;           (hs-hide-leafs-recursive (point-min) (point-max)))
;;         (message "Hiding blocks ... done"))
;;       (run-hooks 'hs-hide-hook)))

(global-set-key (kbd "<backtab>") 'hs-toggle-hiding)
(define-key python-mode-map (kbd "<backtab>") nil)

;; python mode settings
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 2))))
