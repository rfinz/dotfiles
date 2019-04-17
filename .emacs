;;; rfinz --- a dot emacs file

;;; Commentary:

;;; Code:

;; PACKAGES ;;

;; use package manager and add archives
(require 'package)
(setq package-archives
    '(("melpa" . "http://melpa.milkbox.net/packages/")
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
                     rust-mode
                     groovy-mode
                     clojure-mode
                     cider
                     arduino-mode
                     yaml-mode
                     zenburn-theme
                     expand-region
                     multiple-cursors
                     flycheck
                     exec-path-from-shell
                     pyvenv
                     ob-ipython
                     move-text
                     neotree
                     wc-mode
                     monokai-theme
                     magit
                     magit-gitflow
                     projectile
                     paredit
                     paredit-everywhere
                     company
                     diminish
                     flx-ido
                     ag
                     ;;frame-cmds
                     evil
                     org
                     htmlize
                     unfill
                     challenger-deep-theme))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; MODES ;;
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(require 's)

;; Emacs-Lisp
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode t)))

;; Clojure
(add-hook 'clojure-mode-hook (lambda () (paredit-mode t)))

;; making paredit work with delete-selection-mode
; http://whattheemacsd.com/setup-paredit.el-03.html
(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-open-round 'delete-selection t)
(put 'paredit-open-square 'delete-selection t)
(put 'paredit-doublequote 'delete-selection t)
(put 'paredit-newline 'delete-selection t)

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

;; Latex Mode
(add-hook 'latex-mode-hook 'flyspell-mode)
(add-hook 'latex-mode-hook 'wc-mode)

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

(require 'unfill)

(require 'ido)
(ido-mode t)
(flx-ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(mode-enabled save))
(setq-default flycheck-emacs-lisp-load-path 'inherit)
(global-set-key (kbd "C-.") 'flycheck-mode)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require 'neotree)
(global-set-key (kbd "<f8>") 'neotree-toggle)

(require 'magit)
(global-set-key (kbd "<f7>") 'magit-status)
(defvar magit-push-always-verify)
(setq magit-push-always-verify 0)

(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

(require 'pyvenv)
(add-hook 'python-mode-hook 'pyvenv-mode)

(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

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
(add-hook 'org-mode-hook 'wc-mode)
(setq org-default-notes-file (concat org-directory "/capture.org"))
(global-set-key (kbd "<f9>") 'org-capture)

(defvar org-capture-templates)
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
  "Interface to org-open-file-with-emacs that takes PATH.
Extra processing can be done if necessary."
  (org-open-file-with-emacs path))

(defun org-custom-link-post-export (path desc format)
  "Format website link from org file path: takes PATH, DESC, FORMAT."
  (cond
   ((eq format 'html)
    (format "<a href=\"{%% post_url %s %%}\">%s</a>" (s-chop-suffix ".org" path) desc))))

(org-link-set-parameters "post"
                         :follow 'org-custom-link-post-follow
                         :export 'org-custom-link-post-export)

(require 'ob-ipython)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ipython . t)
   ))

;; src block behavior
(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil
      org-edit-src-content-indentation 0)

;; Display/update images in the buffer after evaluation
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)



;; THEMES ;;

(load-theme 'monokai t)

;; USEABILITY ;;

(column-number-mode 1)
(delete-selection-mode 1)
(visual-line-mode 1)
(save-place-mode 1)
(setq scroll-error-top-bottom 1)


(require 'paren)
(show-paren-mode 1)
(setq show-paren-delay 0)
(set-face-attribute 'show-paren-match nil
                    :weight 'bold
                    :foreground "#fd5ff0" ;monokai magenta
                    :background (face-attribute 'default :background)
                    :inverse-video nil)

(add-hook 'write-contents-functions 'delete-trailing-whitespace)
(setq require-final-newline t)

;; Increase garbage-collection threshold
(setq gc-cons-threshold 20000000)

;; Increase max variable bindings
(setq max-specpdl-size 13000)

;; Increase eval depth
(setq max-lisp-eval-depth 20000)

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

;; Toggle Fill/Unfill
(global-set-key (kbd "M-q") 'unfill-toggle)

;; Remap set-fill-column to find-file
(global-set-key "\C-x\ f" 'find-file)

;; Make completion buffers in a shell disappear after 10 seconds.
; http://snarfed.org/space/why+I+don't+run+shells+inside+Emacs
; via Gwern - https://en.wikipedia.org/wiki/User%3AGwern%2F.emacs
(add-hook 'completion-setup-hook
          (lambda () (run-at-time 10 nil
                                  (lambda ()
                                    (delete-windows-on "*Completions*")
                                    (kill-buffer "*Completions*")
                                    ))))

;; Turns tabs into spaces
;; http://www.jwz.org/doc/tabs-vs-spaces.html improved by Claus Brunzem
; via Gwern
(defun ska-untabify ()
  "Untabify whole buffer."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "\t" nil t)
      (untabify (1- (point)) (point-max)))
    nil))

;; Activate untabify for all prog-modes
(add-hook 'prog-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (add-hook 'write-contents-functions 'ska-untabify)))

(defun kill-dired-buffers ()
  "Kill all Dired Buffers."
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(defun kill-ag-buffers ()
  "Kill all Ag buffers."
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'ag-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(defun kill-magit-buffers ()
  "Kill all extra Magit buffers."
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'magit-diff-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer))
          (when (eq 'magit-revision-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer))
          (when (eq 'magit-process-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer))
          )
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

(require 'rfinz-server)

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
(defvar highlight-changes-visibility-initial-state)
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
(setq initial-major-mode 'org-mode) ; sorry Gwern

(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(setq visible-bell t
      ring-bell-function 'flash-mode-line)
(defun flash-mode-line ()
  "Emacswiki function to flash the mode line."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

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
(diminish 'auto-revert-mode)
(eval-after-load "company" '(diminish 'company-mode))
(eval-after-load "paredit" '(diminish 'paredit-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))

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
    (cider clojure-mode rust-mode ob-ipython company htmlize groovy-mode unfill yaml-mode js2-mode s save-packages ## org evil frame-cmds ag flx-ido diminish projectile magit-gitflow magit monokai-theme wc-mode neotree move-text pyvenv exec-path-from-shell flycheck multiple-cursors expand-region zenburn-theme arduino-mode haskell-mode web-mode markdown-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
