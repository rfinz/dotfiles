;;; rfinz --- a dot emacs file
(add-to-list 'load-path "~/.emacs.d")

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
		     flycheck))
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
(eval-after-load 'web-mode
  '(progn
     (define-key web-mode-map (kbd "M-<down>") 'web-mode-tag-next)
     (define-key web-mode-map (kbd "M-<up>") 'web-mode-tag-previous)))


(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdwn\\'" . markdown-mode))

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

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(mode-enabled save))
(global-set-key (kbd "C-.") 'flycheck-mode)



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
(blink-cursor-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
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
	     (* 100 (frame-char-width)))
	  2))
      (setcdr (assq 'continuation fringe-indicator-alist)
	    '(nil nil)))))

(global-set-key (kbd "C-`") 'bzg-big-fringe-mode)

;; Get rid of the indicators in the fringe
;;(mapcar (lambda(fb) (set-fringe-bitmap-face fb 'org-hide))
;; fringe-bitmaps)
