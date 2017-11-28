(setq debug-on-error nil)
(load "server")
(unless (server-running-p) (server-start))

(setq is-mac (equal system-type 'darwin))
(setq is-win (equal system-type 'windows-nt))
(setq is-linux (string-equal system-type "gnu/linux"))

(when is-linux
  ;; (set-default-font "Inconsolata-g 11"))
  ;; emacs cannot handle the dash in the font name, so I created a copy
  (set-default-font "Inconsolata_g 11")
  (setq buffer-face-mode-face '(:family "Noto" :height 120 :weight medium))
  )
(when is-mac
  (set-default-font "-apple-Monaco-medium-normal-normal-*-10-*-*-*-m-0-iso10646-1")
  )
(when is-win
  (set-default-font "Consolas-12")
  (setq buffer-face-mode-face '(:family "Noto" :height 120 :weight light))
  (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
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

(global-set-key (kbd "C-M-v") (lambda() (interactive) (scroll-other-window  4)))
(global-set-key (kbd "S-C-M-v") (lambda() (interactive) (scroll-other-window -4)))

(global-set-key (kbd "<S-return>") (kbd "C-e C-m"))

(defun config() (interactive) (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f9>") 'config)

(defun reload-config()
  "reload your .emacs file without restarting Emacs"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(global-set-key (kbd "<f10>") 'reload-config)

;; Auto-wrap search
;; Basically no longer necessary because we use swiper now
;; (defadvice isearch-search (after isearch-no-fail activate)
;;   (unless isearch-success
;;     (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
;;     (ad-activate 'isearch-search)
;;     (isearch-repeat (if isearch-forward 'forward))
;;     (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
;;     (ad-activate 'isearch-search)))

;; Create empty buffers with expected major mode
;; https://emacs.stackexchange.com/questions/2497/how-to-get-buffers-not-just-files-to-honor-auto-mode-alist/2555#2555
;; http://thread.gmane.org/gmane.emacs.devel/115520/focus=115794
(setq-default major-mode
			  (lambda () (if buffer-file-name
							 (fundamental-mode)
						   (let ((buffer-file-name (buffer-name)))
							 (set-auto-mode)))))

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

(use-package monokai-theme
  :config (load-theme 'monokai t))

;; Line numbering
(use-package nlinum)
(use-package nlinum-hl)
;; (global-nlinum-mode 1)
(setq nlinum-format "%4d")
(add-hook 'prog-mode-hook
		  (lambda()
			(nlinum-mode 1)))

(use-package anzu)
(global-anzu-mode t)

(use-package expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;==============================================================================
;; ivy/swiper
;;==============================================================================

(use-package flx)
(use-package counsel)

(use-package ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
;; (setq enable-recursive-minibuffers t)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-wrap t)
(setq ivy-initial-inputs-alist nil)
(setq ivy-height 13)
(setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
		(counsel-rg . ivy--regex-plus)
        (t      . ivy--regex-fuzzy)))

(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

(define-key ivy-minibuffer-map (kbd "<M-up>") 'ivy-scroll-down-command)
(define-key ivy-minibuffer-map (kbd "<M-down>") 'ivy-scroll-up-command)

;;==============================================================================
;; ido
;;==============================================================================

;; (use-package flx-ido)
;; (ido-mode t)
;; (ido-everywhere t)
;; (flx-ido-mode t)
;; ;; disable ido faces to see flx highlights.
;; (setq ido-enable-flex-matching t)
;; (setq ido-use-faces nil)

;; (use-package ido-vertical-mode)
;; (ido-vertical-mode t)
;; (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

;; (use-package ido-completing-read+)
;; (ido-ubiquitous-mode t)
;; (setq ido-ubiquitous-auto-update-overrides t)

;; (use-package smex)
;; ;; Can be omitted. This might cause a (minimal) delay
;; ;; when Smex is auto-initialized on its first run.
;; (smex-initialize) 
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; ;; This is your old M-x.
;; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;==============================================================================
;; ace-window
;;==============================================================================
;; (use-package ace-window)
;; (global-set-key (kbd "M-p") 'ace-window)

;;==============================================================================
;; iedit
;;==============================================================================

(use-package iedit)

;;==============================================================================
;; dumb-jump
;;==============================================================================

(use-package dumb-jump)
(dumb-jump-mode)
(setq dumb-jump-selector 'ivy)
(setq dumb-jump-force-searcher 'rg)
;; (setq dumb-jump-prefer-searcher 'rg)

;;==============================================================================
;; company
;;==============================================================================

(use-package company)
(add-hook 'after-init-hook 'global-company-mode)


(with-eval-after-load 'company
  (define-key company-active-map (kbd "<escape>") #'company-abort)
  '(add-to-list 'company-backends 'company-cmake)
  )

;; Security-wise this is stupid
;; But I didn't find another way to set company-clang-arguments in .dir-locals.el
(setq enable-local-variables :all)

;;==============================================================================
;; dired-mode
;;==============================================================================

;; List directories before files

(defun mydired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header 
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defadvice dired-readin
  (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding marks."
  (mydired-sort))

;;==============================================================================
;; Defaults for occur and multi-occur
;;==============================================================================

(defun pick-default()
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history))

(defun occur-default()
  "Call `occur' with a sane default."
  (interactive)
  (pick-default)
  (call-interactively 'occur))

(bind-key "M-s o" 'occur-default)

(defun multi-occur-default()
  "Call `multi-occur' with a sane default."
  (interactive)
  (pick-default)
  (call-interactively 'multi-occur))

(bind-key "M-s M-o" 'multi-occur-default)

;;==============================================================================

(use-package ag)
(setq ag-highlight-search t)

(add-hook 'inferior-python-mode-hook
		  (lambda()
			(nlinum-mode 0)
			(company-mode 0)))

(require 'flyspell)

(defun docmode()
  (flyspell-mode 1)
  (setq truncate-lines nil)
  (setq word-wrap 1)) 

(add-hook 'org-mode-hook 'docmode)
(add-hook 'gfm-mode-hook 'docmode)
(add-hook 'markdown-mode-hook 'docmode)
(add-hook 'text-mode-hook 'docmode)

(auto-image-file-mode t)
(add-hook 'image-mode-hook
		  (lambda()
			(company-mode 0)
			(anzu-mode 0)))

(use-package web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;;==============================================================================
;; json
;;==============================================================================

(use-package json-mode)
(add-to-list 'auto-mode-alist '("\\.material\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.scene\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.take\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.effect\\'" . json-mode))

;; Color HTML style hex-color strings 
(defvar hexcolor-keywords
  '(("#[[:xdigit:]]\\{6\\}"
	 (0 (put-text-property
		 (match-beginning 0)
		 (match-end 0)
		 'face (list :background (match-string-no-properties 0)
					 :foreground (if (>= (apply '+ (x-color-values 
													(match-string-no-properties 0)))
										 (* (apply '+ (x-color-values "white")) .6))
									 "black" ;; light bg, dark text
								   "white" ;; dark bg, light text
								   )))))))

(defun hexcolor-add-to-font-lock ()
  (font-lock-add-keywords nil hexcolor-keywords))

(add-hook 'json-mode-hook 'hexcolor-add-to-font-lock)

;;==============================================================================
;; yaml
;;==============================================================================

(use-package yaml-mode)
(add-hook 'yaml-mode-hook
        (lambda ()
		  (nlinum-mode 1)
		  (flyspell-mode 0)
		  ))

;;==============================================================================
;; C++
;;==============================================================================

(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))

(add-hook 'c-mode-common-hook
		  (lambda() 
			(local-set-key [f4] 'ff-find-other-file)))

(defvar my-cpp-other-file-alist
  '(("\\.cpp\\'" (".hpp" ".inl"))
    ("\\.inl\\'" (".hpp" ".cpp"))
    ("\\.hpp\\'" (".cpp" ".inl"))
    ("\\.c\\'" (".h"))
    ("\\.h\\'" (".c"))
    ))

(setq-default ff-other-file-alist 'my-cpp-other-file-alist)

;; Add underscore to word definition
(modify-syntax-entry ?_ "w" (standard-syntax-table))
(add-hook 'c-mode-common-hook
		  (lambda ()
			(modify-syntax-entry ?_ "w")))
(add-hook 'js-mode-hook
		  (lambda ()
			(modify-syntax-entry ?_ "w")))

(use-package glsl-mode)
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vs\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.fs\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.brdf\\'" . c++-mode))

;;==============================================================================
;; cmake-mode
;;==============================================================================

(use-package cmake-mode)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

(setq cmake-tab-width 4)

;;==============================================================================
;; markdown-mode
;;==============================================================================

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;==============================================================================
;; org-mode
;;==============================================================================
(setq org-support-shift-select t
	  org-startup-folded nil
	  org-confirm-babel-evaluate nil
	  org-src-fontify-natively t
	  org-edit-src-content-indentation 0
	  org-src-tab-acts-natively t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

;; NO spell check for embedded snippets
(defadvice org-mode-flyspell-verify (after org-mode-flyspell-verify-hack activate)
  (let* ((rlt ad-return-value)
         (begin-regexp "^[ \t]*#\\+begin_\\(src\\|html\\|latex\\|example\\|quote\\)")
         (end-regexp "^[ \t]*#\\+end_\\(src\\|html\\|latex\\|example\\|quote\\)")
         (case-fold-search t)
         b e)
    (when ad-return-value
      (save-excursion
        (setq b (re-search-backward begin-regexp nil t))
        (if b (setq e (re-search-forward end-regexp nil t))))
      (if (and b e (< (point) e)) (setq rlt nil)))
    (setq ad-return-value rlt)))

;; auto-refresh inline images
(defun shk-fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(add-hook 'org-babel-after-execute-hook 'shk-fix-inline-images)

;; Some key remappings
(add-hook 'org-mode-hook
		  (lambda ()
			;; Make meta up/down behave as in all other modes (as per an earlier global setting)
			;; and re-assign org-metadown/-metaup to control meta down/up keys
			(local-set-key [(meta up)] (lambda () (interactive) (scroll-down 4)))
			(local-set-key [(meta down)] (lambda () (interactive) (scroll-up 4)))
			(local-set-key [(control meta down)] 'org-metadown)
			(local-set-key [(control meta up)] 'org-metaup)
			;; ' does not work well with international keyboard layout
			(local-set-key (kbd "C-c ;") 'org-edit-special)
			))

(add-hook 'org-src-mode-hook
		  (lambda ()
			;; ' does not work well with international keyboard layout
			(local-set-key (kbd "C-c ;") 'org-edit-src-exit)
			))

;;==============================================================================
;; magit
;;==============================================================================

(use-package magit)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;;==============================================================================
;; delete-selection-mode
;;==============================================================================

;; For some reason "delete-selection-mode" gets disabled again under Linux
;; if placed near the top of the file
(delete-selection-mode t)

;;==============================================================================
;; Stuff appended by emacs that we don't really want
;;==============================================================================
