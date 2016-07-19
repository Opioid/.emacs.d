(load "server")
(unless (server-running-p) (server-start))

(setq is-mac (equal system-type 'darwin))
(setq is-win (equal system-type 'windows-nt))
(setq is-linux (string-equal system-type "gnu/linux"))

(when is-mac
	(set-frame-font "-apple-Monaco-medium-normal-normal-*-10-*-*-*-m-0-iso10646-1"))
(when is-win
  (set-frame-font "Consolas-12"))
(when is-linux
  ;; set-default-font was the only way I tried that can load Inconsolata-g repeatedly
  ;; refreshing the config will still fail though
  (set-default-font "Inconsolata-g 11"))

(desktop-save-mode 1)
(tool-bar-mode -1) 
(setq-default tab-width 4)

(delete-selection-mode 1)
(electric-pair-mode 1)

(setq-default cursor-type 'bar)

(set-default 'truncate-lines t)

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

