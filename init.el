(setq debug-on-error nil)
(load "server")
(unless (server-running-p) (server-start))

(setq is-mac (equal system-type 'darwin))
(setq is-win (equal system-type 'windows-nt))
(setq is-linux (string-equal system-type "gnu/linux"))

(when is-mac
  (set-default-font "-apple-Monaco-medium-normal-normal-*-10-*-*-*-m-0-iso10646-1"))
(when is-win
  (set-default-font "Consolas-12")
  (setq buffer-face-mode-face '(:family "Noto" :height 120 :weight light)))
(when is-linux
  ;; (set-default-font "Inconsolata-g 11"))
  ;; emacs cannot handle the dash in the font name, so I created a copy
  (set-default-font "Inconsolata_g 11"))

(desktop-save-mode 1)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq-default tab-width 4)
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
(delete-selection-mode 1)
(electric-pair-mode 1)
(setq-default cursor-type 'bar)
;; (set-default 'truncate-lines t)
(defalias 'yes-or-no-p 'y-or-n-p) ;; confirm with y instead of yes<ret>
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "<M-up>") (lambda () (interactive) (scroll-down 4)))
(global-set-key (kbd "<M-down>") (lambda () (interactive) (scroll-up 4)))

(defun config () (interactive) (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f10>") 'config)

(defun reload-config ()
  "reload your .emacs file without restarting Emacs"
  (interactive)
  (load-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f9>") 'reload-config)

(require 'package)
(setq package-enable-at-startup nil)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
			 '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

(use-package monokai-theme
  :config (load-theme 'monokai t))

(use-package json-mode)
(add-to-list 'auto-mode-alist '("\\.material\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.scene\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.take\\'" . json-mode))

;; Line numbering
;; (global-linum-mode 1)
;; (setq linum-format "%4d")
;; The above is the built-in way (?) but much slower for large files
(use-package nlinum)
(global-nlinum-mode 1)
(setq nlinum-format "%4d")

;; (use-package icicles)
;; (icy-mode 1)
;; icicles seemed a bit too much for me 
(ido-mode t)

(use-package auto-complete)
(ac-config-default)

(use-package adoc-mode)
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))
(add-hook 'adoc-mode-hook (lambda()
							(buffer-face-mode t)
							(nlinum-mode 0)))
