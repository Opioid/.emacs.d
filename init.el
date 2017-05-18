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
  (setq buffer-face-mode-face '(:family "Noto" :height 120 :weight light))
  (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/"))
(when is-linux
  ;; (set-default-font "Inconsolata-g 11"))
  ;; emacs cannot handle the dash in the font name, so I created a copy
  (set-default-font "Inconsolata_g 11")
  (setq buffer-face-mode-face '(:family "Noto" :height 120 :weight medium))
  )

(setq-default line-spacing 1)

(setq gc-cons-threshold 20000000) ;; Garbage collection to ca. 20 MB
(desktop-save-mode 1)
(global-auto-revert-mode t) ;; Update files changed fro manother process
(setq
 backup-by-copying t ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves")) ; don't litter my fs tree
 )   

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq horizontal-scroll-bars nil)
;; This was neccessary to get rid of the horizontal scroll-bars
;; (modify-all-frames-parameters '((horizontal-scroll-bars . nil)))

(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
(setq sgml-basic-offset 4) ;; In my case basically tab-width for HTML
(setq-default c-basic-offset 4)
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
;; (setq sentence-end-double-space nil)
(electric-pair-mode 1)
(show-paren-mode 1)
(setq-default cursor-type 'bar)
(set-default 'truncate-lines t)
(defalias 'yes-or-no-p 'y-or-n-p) ;; confirm with y instead of yes<ret>
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "<M-up>") (lambda () (interactive) (scroll-down 4)))
(global-set-key (kbd "<M-down>") (lambda () (interactive) (scroll-up 4)))

(defun config() (interactive) (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f10>") 'config)

(defun reload-config()
  "reload your .emacs file without restarting Emacs"
  (interactive)
  (load-file "~/.emacs.d/init.el")
  )
(global-set-key (kbd "<f9>") 'reload-config)

;; Auto-wrap search
(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
	)
  )

(require 'package)
(setq package-enable-at-startup nil)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  )

(setq use-package-always-ensure t)

(use-package monokai-theme
  :config (load-theme 'monokai t))

(use-package json-mode)
(add-to-list 'auto-mode-alist '("\\.material\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.scene\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.take\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.effect\\'" . json-mode))

;; C++ files
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))

(add-hook 'c-mode-common-hook
		  (lambda() 
			(local-set-key [f4] 'ff-find-other-file)))

;; Add underscore to word definition
(modify-syntax-entry ?_ "w" (standard-syntax-table))
(add-hook 'c-mode-common-hook
		  (lambda ()
			(modify-syntax-entry ?_ "w")))
(add-hook 'js-mode-hook
		  (lambda ()
			(modify-syntax-entry ?_ "w")))

;; Could not find glsl mode
;; (use-package glsl-mode)
(add-to-list 'auto-mode-alist '("\\.vert\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.brdf\\'" . c++-mode))

;; (use-package cmake-mode)
;; (add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
;; (add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

;; Line numbering
;; (global-linum-mode 1)
;; (setq linum-format "%4d")
;; The above is the built-in way (?) but much slower for large files
(use-package nlinum)
(global-nlinum-mode 1)
(setq nlinum-format "%4d")

(use-package flx-ido)
(ido-mode t)
(ido-everywhere t)
(flx-ido-mode t)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(use-package ido-vertical-mode)
(ido-vertical-mode t)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

(use-package ido-ubiquitous)
(ido-ubiquitous-mode t)
(setq ido-ubiquitous-auto-update-overrides t)

(use-package smex)
;; Can be omitted. This might cause a (minimal) delay
;; when Smex is auto-initialized on its first run.
(smex-initialize) 
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(use-package company)
(add-hook 'after-init-hook 'global-company-mode)

(use-package anzu)
(global-anzu-mode t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
		 )
  :init (setq markdown-command "multimarkdown")
  )

(require 'ispell)
;; (setq ispell-extra-args '("--sug-mode=fast"))

(defun docmode()
  (nlinum-mode 0)
  (flyspell-mode 1)
  ) 

(add-hook 'org-mode-hook 'docmode)
(add-hook 'gfm-mode-hook 'docmode)
(add-hook 'markdown-mode-hook 'docmode)
(add-hook 'text-mode-hook 'docmode)

(setq org-support-shift-select t)
(setq org-startup-folded nil)

(auto-image-file-mode t)
(add-hook 'image-mode-hook
		  (lambda()
			(nlinum-mode 0)
			(company-mode 0)
			(anzu-mode 0)
			))

;;(use-package highlight-thing)
;;(global-highlight-thing-mode)

;;(use-package auto-highlight-symbol)
;;(global-auto-highlight-symbol-mode t)

(use-package highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)
;; (global-set-key [double-down-mouse-1] 'highlight-symbol)

;; For some reason "delete-selection-mode" gets disabled again under Linux if placed near the top of the file
(delete-selection-mode t)

