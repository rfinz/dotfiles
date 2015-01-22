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
(setq package-list '(markdown-mode
		     web-mode
		     haskell-mode
		     zenburn-theme
		     expand-region
		     multiple-cursors
		     flycheck
		     exec-path-from-shell
		     pyvenv
		     move-text
		     neotree
		     wc-mode
		     monokai-theme))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))



;; MODES ;;

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(setq web-mode-engines-alist '(("django" . "\\.html?\\'")) )

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
(setq ido-enable-flex-matching t)

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(mode-enabled save))
(global-set-key (kbd "C-.") 'flycheck-mode)

(require 'neotree)
(global-set-key (kbd "<f8>") 'neotree-toggle)

(require 'pyvenv)
(add-hook 'python-mode-hook 'pyvenv-mode)

(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

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

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

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

;; Python keybindings
(defun rfinz-python-keys ()
  "My personal preferences for python."
  (local-set-key (kbd "M-<up>") 'move-text-up)
  (local-set-key (kbd "M-<down>") 'move-text-down))
(add-hook 'python-mode-hook 'rfinz-python-keys)

;; Web keybindings
(defun rfinz-web-keys ()
  "My personal preferences for web development."
  (local-set-key (kbd "M-<up>") 'web-mode-block-previous)
  (local-set-key (kbd "M-<down>") 'web-mode-block-next))
(add-hook 'web-mode-hook 'rfinz-web-keys)

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
(setq inhibit-startup-message t)
(setq visible-bell t)

;; BZG Big Fringe Mode - tiny mode
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
      (set-fringe-mode
       (/ (- (frame-pixel-width)
	     (* 82 (frame-char-width)))
	  2))
      (setcdr (assq 'continuation fringe-indicator-alist)
	    '(nil nil)))))

(global-set-key (kbd "C-`") 'bzg-big-fringe-mode)

;; Alpha-mode
;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
;; (set-frame-parameter (selected-frame) 'alpha '(85 50))
;; (add-to-list 'default-frame-alist '(alpha 85 50))
;; http://www.emacswiki.org/emacs/TransparentEmacs


;; Get rid of the indicators in the fringe
;;(mapcar (lambda(fb) (set-fringe-bitmap-face fb 'org-hide))
;; fringe-bitmaps)

(provide '.emacs)

;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4e262566c3d57706c70e403d440146a5440de056dfaeb3062f004da1711d83fc" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
