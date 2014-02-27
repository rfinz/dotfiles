(add-to-list 'load-path "~/.emacs.d")


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

(setq package-list '(markdown-mode
		     web-mode 
		     haskell-mode 
		     zenburn-theme 
		     expand-region))
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

(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdwn\\'" . markdown-mode))

(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; THEMES ;;

(when (display-graphic-p)
  (load-theme 'zenburn t))

;; USEABILITY ;;

(show-paren-mode 1)
(setq show-paren-delay 0)
(set-face-background 'show-paren-match-face (face-background 'default))
(set-face-foreground 'show-paren-match-face "#ff00aa")
(set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)


(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; GUI STRIPTEASE ;;
;  Bastien Guerry
;  http://bzg.fr/emacs-strip-tease.html
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
	(mapcar (lambda(fb) (set-fringe-bitmap-face fb 'org-hide))
		fringe-bitmaps)
	)
    (set-fringe-mode
     (/ (- (frame-pixel-width)
           (* 100 (frame-char-width)))
        2))))
(global-set-key (kbd "C-`") 'bzg-big-fringe-mode)

;; Get rid of the indicators in the fringe
;;(mapcar (lambda(fb) (set-fringe-bitmap-face fb 'org-hide))
;;       fringe-bitmaps)
