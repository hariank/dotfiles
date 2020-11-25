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
(projectile-add-known-project "~/dotfiles")
(projectile-add-known-project "~/Dropbox/private/roam/")
(projectile-add-known-project (getenv "SOURCE"))

(setq org-directory "~/Dropbox/private/roam/")
(setq org-default-notes-file "~/Dropbox/private/roam/inbox.org")
(setq org-agenda-files '("~/Dropbox/private/roam/inbox.org"
                         "~/Dropbox/private/roam/projects.org"
                         "~/Dropbox/private/roam/backlog.org"))

(setq org-roam-directory "/Users/hmuthakana/Dropbox/private/roam/")
(setq org-roam-link-title-format "%s")
(setq org-roam-buffer-width 0.25)

(setq org-agenda-custom-commands '(("ww" "Work tasks" tags-todo "+work"
                                   ((org-agenda-files '("~/Dropbox/private/roam/projects.org")) (org-agenda-sorting-strategy '(todo-state-down))))
                                   ("ws" "Side tasks" tags-todo "+side"
                                   ((org-agenda-files '("~/Dropbox/private/roam/projects.org")) (org-agenda-sorting-strategy '(todo-state-down))))
                                   ("wh" "home tasks" tags-todo "+home"
                                   ((org-agenda-files '("~/Dropbox/private/roam/projects.org")) (org-agenda-sorting-strategy '(todo-state-down))))
                                   ("bw" "Backlog work tasks" tags-todo "+work"
                                   ((org-agenda-files '("~/Dropbox/private/roam/backlog.org")) (org-agenda-sorting-strategy '(todo-state-down))))
                                   ("bs" "Backlog nonwork tasks" tags-todo "-work"
                                   ((org-agenda-files '("~/Dropbox/private/roam/backlog.org")) (org-agenda-sorting-strategy '(todo-state-down))))
                                   ("f" "Fitlog" tags "+fitlog"
                                   ((org-agenda-files '("~/Dropbox/private/roam/"))))))

;; wrap by default, but don't wrap in agenda mode - currently not working
;; (global-visual-line-mode t) ;; visual line wrap
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

;; autosave all org buffers on agenda quit
(advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)


;; customize how org nesting looks
(setq org-n-level-faces `1)
(custom-theme-set-faces 'user
                        `(org-level-1 ((t (:foreground "grey")))))

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

;; autocomplete
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

;; (global-set-key (kbd "<backtab>") 'hs-toggle-hiding)
;; (define-key python-mode-map (kbd "<backtab>") nil)

;; python mode settings
(defun my-python-mode-hook ()
  (setq indent-tabs-mode nil tab-width 2)
  ;; (idle-highlight-mode t)
)
(add-hook 'python-mode-hook 'my-python-mode-hook)

(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:foreground "grey")))))

;; doom prompts speeedup and cleanup
(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))

(defun xah-copy-file-path (&optional @dir-path-only-p)
"Copy the current buffer's full file path or dired path to `kill-ring'.
If in dired, copy the file/dir cursor is on, or marked files.
URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'"
  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result (mapconcat 'identity (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory )
                   (progn $result))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if @dir-path-only-p
         (progn
           (message "Directory path copied: 「%s」" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: 「%s」" $fpath)
         $fpath )))))
