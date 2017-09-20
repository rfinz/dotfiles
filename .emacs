;;; rfinz --- a dot emacs file
(add-to-list 'load-path "~/.emacs.d/lisp")

;;; Commentary:

;;; Code:

;; PACKAGES ;;

;; use package manager and add archives
(require 'package)
(setq package-archives
    '(("marmalade" . "http://marmalade-repo.org/packages/")
      ("melpa" . "http://melpa.milkbox.net/packages/")
      ("gnu" . "http://elpa.gnu.org/packages/")))

;; refresh package list
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; initialize package manager and install missing packages
(package-initialize)
(defvar package-list)
(setq package-list '(s
		     markdown-mode
		     web-mode
		     js2-mode
		     haskell-mode
		     arduino-mode
		     zenburn-theme
		     expand-region
		     multiple-cursors
		     flycheck
		     exec-path-from-shell
		     pyvenv
		     move-text
		     neotree
		     wc-mode
		     monokai-theme
		     magit
		     magit-gitflow
		     projectile
		     diminish
		     flx-ido
		     ag
		     frame-cmds
		     evil
		     org))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))



;; MODES ;;
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(require 's)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(setq web-mode-engines-alist '(("django" . "\\.html?\\'")) )

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(require 'wc-mode)
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdwn\\'" . markdown-mode))
(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'wc-mode)

(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(require 'ido)
(ido-mode t)
(flx-ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(mode-enabled save))
(global-set-key (kbd "C-.") 'flycheck-mode)

(require 'neotree)
(global-set-key (kbd "<f8>") 'neotree-toggle)

(require 'magit)
(global-set-key (kbd "<f7>") 'magit-status)
(setq magit-push-always-verify 0)

(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

(require 'pyvenv)
(add-hook 'python-mode-hook 'pyvenv-mode)

(require 'projectile)
(projectile-mode)

(require 'frame-cmds)
(global-set-key (kbd "<s-up>") 'move-frame-up)
(global-set-key (kbd "<s-down>") 'move-frame-down)
(global-set-key (kbd "<s-left>") 'move-frame-left)
(global-set-key (kbd "<s-right>") 'move-frame-right)
(global-set-key (kbd "s-p") 'move-frame-up)
(global-set-key (kbd "s-n") 'move-frame-down)
(global-set-key (kbd "s-b") 'move-frame-left)
(global-set-key (kbd "s-f") 'move-frame-right)

(require 'org)
(require 'ox-publish)
(add-hook 'org-mode-hook 'flyspell-mode)
(setq org-default-notes-file (concat org-directory "/capture.org"))
(global-set-key (kbd "<f9>") 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat org-directory "/gtd.org") "Tasks")
             "* TODO %?\n  %i\n  %a" :prepend t)
        ("s" "Store" entry (file+datetree (concat org-directory "/store.org"))
	 "* %?\nEntered on %U\n  %i\n  %a" :prepend t)
	("c" "Clock" table-line (file (concat org-directory "/timetracking.org"))
	 "| %?%U | %U | '%^{Project}' |  | %^{Work Type} | %^{Notes} |" :table-line-pos "I+1")
	))
(add-hook 'org-agenda-mode-hook
	  (lambda ()
	    (add-hook 'projectile-mode-hook
		      (setq org-agenda-files '(".")) t)))
(setq org-publish-project-alist
      '(
	("org-rfinz"
	 :base-directory "~/Projects/rfinz/org/"
	 :base-extension "org"
	 :publishing-directory "~/Projects/rfinz/jekyll"
	 :recursive t
	 :publishing-function org-html-publish-to-html
	 :headline-levels 4
	 :html-extension "html"
	 :body-only t
	 )

	("org-static-rfinz"
	 :base-directory "~/Projects/rfinz/org/"
	 :base-extension "html\\|css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
	 :publishing-directory "~/Projects/rfinz/jekyll"
	 :recursive t
	 :publishing-function org-publish-attachment
	 )

	("rfinz" :components ("org-rfinz" "org-static-rfinz"))
	)
      )

(defun org-custom-link-post-follow (path)
  (org-open-file-with-emacs path))

(defun org-custom-link-post-export (path desc format)
  (cond
   ((eq format 'html)
    (format "<a href=\"{%% post_url %s %%}\">%s</a>" (s-chop-suffix ".org" path) desc))))

(org-add-link-type "post" 'org-custom-link-post-follow 'org-custom-link-post-export)


;; THEMES ;;

(when (display-graphic-p)
  (load-theme 'zenburn t))

;; USEABILITY ;;

(column-number-mode 1)

(show-paren-mode 1)
(setq show-paren-delay 0)
(set-face-background 'show-paren-match-face (face-background 'default))
(set-face-foreground 'show-paren-match-face "#ff00aa")
(set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

(delete-selection-mode 1)
(visual-line-mode 1)

;; Increase garbage-collection threshold
(setq gc-cons-threshold 20000000)

;; Bug in Emacs prevents this from being useful
;; ;; Auto refresh buffers
;; (global-auto-revert-mode 1)

;; ;; Also auto refresh dired, but be quiet about it
;; (setq global-auto-revert-non-file-buffers t)
;; (setq auto-revert-verbose nil)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Write autosave files to temp
(setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Firefox style tabbing
(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd "C-S-<iso-lefttab>") 'previous-buffer)
(global-set-key (kbd "C-S-<tab>") 'previous-buffer)

;; Kill all Dired Buffers
(defun kill-dired-buffers ()
  (interactive)
  (mapc (lambda (buffer)
	  (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
	    (kill-buffer buffer)))
	(buffer-list)))

;; Kill all Ag Buffers
(defun kill-ag-buffers ()
  (interactive)
  (mapc (lambda (buffer)
	  (when (eq 'ag-mode (buffer-local-value 'major-mode buffer))
	    (kill-buffer buffer)))
	(buffer-list)))

;; Python keybindings
(defun rfinz-python-hook ()
  "My personal preferences for python."
  (local-set-key (kbd "M-<up>") 'move-text-up)
  (local-set-key (kbd "M-<down>") 'move-text-down))
(add-hook 'python-mode-hook 'rfinz-python-hook)

;; Web keybindings
(defun rfinz-web-hook ()
  "My personal preferences for web development."
  (setq web-mode-markup-indent-offset 2)
  (setq create-lockfiles nil)
  (local-set-key (kbd "M-<up>") 'web-mode-element-previous)
  (local-set-key (kbd "M-<down>") 'web-mode-element-next))
(add-hook 'web-mode-hook 'rfinz-web-hook)

;; Server commands
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )

;; Emacs Wiki switch to minibuffer
; https://www.emacswiki.org/emacs/MiniBuffer
(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(global-set-key "\C-cxo" 'switch-to-minibuffer)


;; EMACS-FU change tracking
; DJCB
; http://emacs-fu.blogspot.com/2009/05/tracking-changes.html
(setq highlight-changes-visibility-initial-state nil)
(global-highlight-changes-mode t)
(set-face-background 'highlight-changes "#7ACC7A")
(set-face-foreground 'highlight-changes (face-background 'default))
(global-set-key (kbd "C-<f6>") 'highlight-changes-visible-mode)
(global-set-key (kbd "C-S-<f6>") 'highlight-changes-remove-highlight)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))


;; GUI STRIPTEASE ;;
; Bastien Guerry
; http://bzg.fr/emacs-strip-tease.html
(blink-cursor-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq initial-scratch-message "")

(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(setq visible-bell 'top-bottom)

;; BZG Big Fringe Mode - tiny mode (edited by rfinz)
(defvar bzg-big-fringe-mode nil)
(define-minor-mode bzg-big-fringe-mode
  "Minor mode to use big fringe in the current buffer."
  :init-value nil
  :global t
  :variable bzg-big-fringe-mode
  :group 'editing-basics
  (if (not bzg-big-fringe-mode)
      (progn
	(set-fringe-style nil)
	(setcdr (assq 'continuation fringe-indicator-alist)
	    '(left-curly-arrow right-curly-arrow)))
    (progn
      (set-fringe-style
       (max (/ (* (- (window-total-width) 80) (frame-char-width)) 2) 8))

      (setcdr (assq 'continuation fringe-indicator-alist)
	    '(nil nil)))))

(global-set-key (kbd "C-`") 'bzg-big-fringe-mode)


;; BZG Hidden Mode Line Mode
;; See http://bzg.fr/emacs-hide-mode-line.html
(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))


;; Alpha-mode
;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
;; (set-frame-parameter (selected-frame) 'alpha '(85 50))
;; (add-to-list 'default-frame-alist '(alpha 85 50))
;; http://www.emacswiki.org/emacs/TransparentEmacs


;; Get rid of the indicators in the fringe
;;(mapcar (lambda(fb) (set-fringe-bitmap-face fb 'org-hide))
;; fringe-bitmaps)

;; Diminish Modes
(require 'diminish)
(diminish 'projectile-mode)
(diminish 'highlight-changes-mode)
(diminish 'magit-gitflow-mode)
(diminish 'visual-line-mode)


;; System Specific
;; (add-to-list 'load-path "/usr/local/bin/sclang")
;; (require 'sclang)


(provide '.emacs)

;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (s save-packages ## org evil frame-cmds ag flx-ido diminish projectile magit-gitflow magit monokai-theme wc-mode neotree move-text pyvenv exec-path-from-shell flycheck multiple-cursors expand-region zenburn-theme arduino-mode haskell-mode web-mode markdown-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
