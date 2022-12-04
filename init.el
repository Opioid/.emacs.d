(setq debug-on-error nil)
(load "server")
(unless (server-running-p) (server-start))

(setq is-mac (equal system-type 'darwin))
(setq is-win (equal system-type 'windows-nt))
(setq is-linux (string-equal system-type "gnu/linux"))

(when is-linux
  (set-frame-font "Source Code Pro 14")
  (setq buffer-face-mode-face '(:family "Noto" :height 140 :weight medium)))
(when is-mac
  (set-frame-font "-apple-Monaco-medium-normal-normal-*-10-*-*-*-m-0-iso10646-1"))
(when is-win
  (set-frame-font "Source Code Pro Light 11")
  (setq buffer-face-mode-face '(:family "Noto" :height 120 :weight light))
  (add-to-list 'exec-path "C:/Program Files (x86)/Hunspell/bin/")
  (setq shell-file-name "c:/Program Files/Git/bin/bash.exe")
  (setq explicit-shell-file-name "c:/Program Files/Git/bin/bash.exe")
  (setq explicit-bash.exe-args '("--login" "-i")))

(setq-default line-spacing 1)

(setq gc-cons-threshold 20000000) ;; Garbage collection to ca. 20 MB
(desktop-save-mode 1)
(global-auto-revert-mode t) ;; Update files changed from another process
(setq
 backup-by-copying t ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves")) ; don't litter my fs tree
 )   

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq horizontal-scroll-bars nil)
(setq visible-bell 1)

(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq sgml-basic-offset 4) ;; In my case basically tab-width for HTML
(setq-default c-basic-offset 4)
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
;; automatically scroll the compilation window
(setq compilation-scroll-output t)
;; Don't save *anything* before compiling (use projectile-save-buffers instead further down)
(setq compilation-save-buffers-predicate 'ignore)
;; (setq sentence-end-double-space nil)
(electric-pair-mode 1)
(show-paren-mode 1)
(setq-default cursor-type 'bar)
(set-default 'truncate-lines t)
;; avoids "Keep current list of tags table also"
(setq tags-add-tables nil)
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

;; Create empty buffers with expected major mode
;; https://emacs.stackexchange.com/questions/2497/how-to-get-buffers-not-just-files-to-honor-auto-mode-alist/2555#2555
;; http://thread.gmane.org/gmane.emacs.devel/115520/focus=115794
(setq-default major-mode
			  (lambda () (if buffer-file-name
							 (fundamental-mode)
						   (let ((buffer-file-name (buffer-name)))
							 (set-auto-mode)))))

;; Windows performance tweaks
(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))
;; Set the buffer size to 64K on Windows (from the original 4K)
;; (when (boundp 'w32-pipe-buffer-size)
;;  (setq irony-server-w32-pipe-buffer-size (* 64 1024)))

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
  :config (load-theme 'monokai t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#32312a")))

;; Line numbering
(setq display-line-numbers-width-start t)
(add-hook 'prog-mode-hook
		  (lambda()
			(display-line-numbers-mode 1)))

(use-package goto-addr
  :hook ((compilation-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)
         (eshell-mode . goto-address-mode)
         (shell-mode . goto-address-mode))
  :bind (:map goto-address-highlight-keymap
              ("<RET>" . goto-address-at-point)
              ("M-<RET>" . newline))
  :commands (goto-address-prog-mode
             goto-address-mode))

(use-package expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(use-package diminish)
(diminish 'abbrev-mode)
(diminish 'server-buffer-clients)
(diminish 'eldoc-mode)

;;==============================================================================
;; calendar
;;==============================================================================
(setq calendar-week-start-day 1)

;;==============================================================================
;; ivy/swiper
;;==============================================================================
(use-package flx)

(use-package ivy
  :diminish ivy-mode
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  ;; (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-wrap t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-height 13)
  (setq ivy-re-builders-alist
		'((swiper . ivy--regex-plus)
		  (counsel-ag . ivy--regex-plus)
          (counsel-rg . ivy--regex-plus)
		  (t . ivy--regex-fuzzy)))
  (setq counsel-find-file-at-point t)
  :bind (:map ivy-minibuffer-map
			  ("M-<up>" . ivy-scroll-down-command)
			  ("M-<down>" . ivy-scroll-up-command)
              ("<return>" . ivy-alt-done)
              ("C-<return>" . ivy-immediate-done)))

(use-package swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("<f3>". swiper-thing-at-point)))

(defun my-minibuffer-keyboard-quit ()
  "Abort recursive edit."
  "In Delete Selection mode, if the mark is active, just deactivate it;"
  "then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(define-key ivy-minibuffer-map (kbd "<f3>") 'my-minibuffer-keyboard-quit)
(define-key ivy-minibuffer-map (kbd "<escape>") 'my-minibuffer-keyboard-quit)

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
		 ("M-y" . counsel-yank-pop)
		 :map ivy-minibuffer-map
		 ("M-y" . ivy-next-line)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history))
  :config
  (if (executable-find "rg")
      ;; use ripgrep instead of grep because it's way faster
      (setq counsel-grep-base-command
            "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
            counsel-rg-base-command
            "rg -i -M 120 --no-heading --line-number --color never %s ."
            )
    (warn "\nWARNING: Could not find the ripgrep executable. It "
          "is recommended you install ripgrep.")))

(use-package shell
  :bind (:map shell-mode-map
              ("C-r" . counsel-shell-history))
  )

(use-package wgrep)
;;==============================================================================
;; mode-line adjustments
;;==============================================================================
(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT aligned respectively."
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format "%%s %%%ds" available-width) left right)))

(setq-default mode-line-format
              '((:eval
                 (simple-mode-line-render
                  ;; left
                  (format-mode-line
                   '("%e"
                     mode-line-front-space
	 	             mode-line-mule-info
	 	             mode-line-client
	 	             mode-line-modified
	 	             mode-line-remote
	 	             mode-line-frame-identification
	 	             mode-line-buffer-identification
	 	             " "
	 	             mode-line-modes
	 	             " "
		             (vc-mode vc-mode)
	 	             mode-line-misc-info
		             mode-line-end-spaces
                     ))

                  ;; right
                  (format-mode-line "%l:%c %p%%")))))

;; Has to come after our customized mode-line render
(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (when is-linux
	(setq moody-mode-line-height 24))
  (when is-win
	(setq moody-mode-line-height 22))
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;;==============================================================================
;; iedit
;;==============================================================================
(use-package iedit)

;;==============================================================================
;; multiple-cursors
;;==============================================================================
(use-package multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;==============================================================================
;; whole-line-or-region
;;==============================================================================
(use-package whole-line-or-region
  :diminish whole-line-or-region-local-mode
  :config
  (whole-line-or-region-global-mode))

;;==============================================================================
;; company
;;==============================================================================
(use-package company
  :diminish company-mode
  :config
  ;; Zero delay when pressing tab
  ;; (setq company-idle-delay 0)
  (setq company-selection-wrap-around t)
  (add-hook 'after-init-hook 'global-company-mode)
  (define-key company-active-map (kbd "<escape>") #'company-abort)
  (setq company-backends (delete 'company-clang company-backends))
  (add-to-list 'company-backends 'company-cmake))

;; Security-wise this is stupid
;; But I didn't find another way to set company-clang-arguments in .dir-locals.el
(setq enable-local-variables :all)

;;==============================================================================
;; dired-mode
;;==============================================================================
(use-package dired)

(use-package dired-single
  :bind (:map dired-mode-map
			  ("<return>" . dired-single-buffer)
              ("<mouse-1>" . dired-single-buffer-mouse)
              ("<backspace>" . dired-single-up-directory)))

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
(defun pick-default ()
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history))

(defun occur-default ()
  "Call `occur' with a sane default."
  (interactive)
  (pick-default)
  (call-interactively 'occur))

(bind-key "M-s o" 'occur-default)

(defun multi-occur-default ()
  "Call `multi-occur' with a sane default."
  (interactive)
  (pick-default)
  (call-interactively 'multi-occur))

(bind-key "M-s M-o" 'multi-occur-default)

;;=============================================================================
;; Rainbow delimiters
;;=============================================================================
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;=============================================================================
;; projectile
;;=============================================================================
;; Directly run the project run command, without further prompt.
;; (defun my-projectile-run-project (&optional prompt)
;;   (interactive "P")
;;   (let ((compilation-read-command
;;          (or (not (projectile-run-command (projectile-compilation-dir)))
;;              prompt)))
;;     (projectile-run-project prompt)))

(defun my-projectile-run-project (&optional prompt)
  (interactive "P")
  (let (compilation-read-command prompt)
    (projectile-save-project-buffers)
    (projectile-run-project prompt)))

;; Directly compile the project, without further prompt.
;; Automatically save all project buffers before.
(defun my-projectile-compile-project (&optional prompt)
  (interactive "P")
  (let (compilation-read-command prompt)
	(projectile-save-project-buffers)
    (projectile-compile-project prompt)))

(defun my-projectile-mode-line ()
  (let ((project-name (projectile-project-name)))
    (format " [%s]" (or project-name "-"))))

(use-package projectile
  :config
  (projectile-mode)
  (setq projectile-mode-line-function 'my-projectile-mode-line)
  (setq projectile-indexing-method 'hybrid)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :bind (([f5] . my-projectile-run-project)
		 ([f7] . my-projectile-compile-project)))

(use-package counsel-projectile
  :config
  (add-to-list 'counsel-projectile-key-bindings '("s" . counsel-projectile-rg) t)
  (counsel-projectile-mode))

;;=============================================================================
;; counsel-etags
;;=============================================================================
(use-package counsel-etags
  :bind (("M-." . counsel-etags-find-tag-at-point)
         ("M-t" . counsel-etags-grep-symbol-at-point)
         ;;("M-s" . counsel-etags-find-tag)
         )
  :config
  ;; Ignore files above 800kb
  (setq counsel-etags-max-file-size 800)
  (setq counsel-etags-debug t)
  ;; Ignore build directories for tagging
  (add-to-list 'counsel-etags-ignore-directories '"build*")
  (add-to-list 'counsel-etags-ignore-directories '"data")
  (add-to-list 'counsel-etags-ignore-directories '"deps")
  (add-to-list 'counsel-etags-ignore-directories '"doc")
  (add-to-list 'counsel-etags-ignore-directories '"extern")
  (add-to-list 'counsel-etags-ignore-directories '"test")
  (add-to-list 'counsel-etags-ignore-directories '"tools")
  (add-to-list 'counsel-etags-ignore-directories '".vscode")
  (add-to-list 'counsel-etags-ignore-filenames '".clang-format")
  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil)
  ;; How many seconds to wait before rerunning tags for auto-update
  (setq counsel-etags-update-interval 180)
  (setq counsel-etags-quiet-when-updating-tags t)
  ;; Set up auto-update
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        'counsel-etags-virtual-update-tags 'append 'local))))

;;==============================================================================
;; flyspell
;;==============================================================================
(use-package flyspell
  :diminish
  :bind (([f8] . flyspell-correct-at-point))  
  :config
  ;;  (add-hook 'text-mode-hook #'flyspell-mode)
  ;;  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  ;; I'm never using thins function, and it interferes with iedit default
  (unbind-key "C-;" flyspell-mode-map))

(use-package flyspell-correct-ivy
  :after flyspell)

;;==============================================================================
;; rainbow-mode
;;==============================================================================
(use-package rainbow-mode
  :diminish)

;;==============================================================================
(add-hook 'inferior-python-mode-hook
		  (lambda()
			(display-line-numbers-mode 0)
			(company-mode 0)))

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
			(company-mode 0)))

(defun shellmode()
  (setq truncate-lines nil)
  (setq word-wrap 1))

(add-hook 'shell-mode-hook 'shellmode)
(add-hook 'compilation-mode-hook 'shellmode)

;;==============================================================================
;; web-mode
;;==============================================================================
(use-package web-mode
  :mode ("\\.html?\\'"))

;;==============================================================================
;; javascript
;;==============================================================================
(use-package js2-mode
  :mode ("\\.js\\'"))

(add-hook 'js-mode-hook
		  (lambda ()
			(modify-syntax-entry ?_ "w")))
;;==============================================================================
;; json
;;==============================================================================
(use-package json-mode
  :mode ("\\.material\\'"
		 "\\.scene\\'"
		 "\\.take\\'"
		 "\\.effect\\'")
  :hook (json-mode . rainbow-mode))

;;==============================================================================
;; yaml
;;==============================================================================
(use-package yaml-mode
  :mode ("\\.clang-format")
  :hook
  (yaml-mode . (lambda ()
				 (display-line-numbers-mode 1)
				 (flyspell-mode 0))))

;;==============================================================================
;; C/C++
;;==============================================================================
;; (use-package irony
;;   :hook ((c++-mode) . irony-mode))

;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; (use-package company-irony)
;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-irony))

(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))

(add-hook 'c-mode-common-hook
		  (lambda() 
			(local-set-key [f4] 'ff-find-other-file)
            (c-set-offset 'brace-list-intro '+)))

(add-hook 'c++-mode-hook
          (lambda()
            (c-set-offset 'innamespace [0])))

(defvar my-cpp-other-file-alist
  '(("\\.cpp\\'" (".hpp" ".h" ".inl"))
    ("\\.inl\\'" (".hpp" ".h" ".cpp"))
    ("\\.hpp\\'" (".cpp" ".inl"))
    ("\\.c\\'" (".h"))
    ("\\.h\\'" (".c" ".cpp"))))

(setq-default ff-other-file-alist 'my-cpp-other-file-alist)

;; Add underscore to word definition
(modify-syntax-entry ?_ "w" (standard-syntax-table))
(add-hook 'c-mode-common-hook
		  (lambda ()
			(modify-syntax-entry ?_ "w")))

;; makes the commenting function a (little) bit similar to qtcrator behavior
(defun my-qtcreator-likeish-comments (orig-fun beg end &optional arg)
  (if (and (derived-mode-p 'c-mode 'c++-mode 'rust-mode)
           (save-excursion
             (goto-char end)
             (not (looking-at-p "[ \t]*$"))))
      (let ((comment-start "/* ")
            (comment-end " */"))
        (funcall orig-fun beg end arg))
    (funcall orig-fun beg end arg)))

(advice-add 'comment-region :around 'my-qtcreator-likeish-comments)

;;==============================================================================
;; rust-mode
;;==============================================================================
(use-package rust-mode
  :mode ("\\.rs\\'"))

;;==============================================================================
;; rust-mode
;;==============================================================================
(use-package zig-mode
  :mode ("\\.zig\\'"))

;;==============================================================================
;; python-mode
;;==============================================================================
(defun my-python-shell-send-buffer ()
  "Save the current buffer before calling `python-shell-send-buffer'."
  (interactive)
  (save-buffer)
  (python-shell-send-buffer))

(add-hook 'python-mode-hook
          (lambda () (define-key python-mode-map (kbd "C-c C-c") 'my-python-shell-send-buffer)))

;;==============================================================================
;; glsl-mode
;;==============================================================================
(use-package glsl-mode
  :mode ("\\.vert\\'"
		 "\\.vs\\'"
		 "\\.frag\\'"
		 "\\.fs\\'"
		 "\\.glsl\\'"
		 "\\.brdf\\'"))

;;==============================================================================
;; shader-mode (Unity)
;;==============================================================================
(use-package shader-mode
  :mode ("\\.shader\\'"
         "\\.cginc\\'"))

;;==============================================================================
;; cmake-mode
;;==============================================================================
(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'"
		 "\\.cmake\\'")
  :custom
  (cmake-tab-width 4))

;;==============================================================================
;; markdown-mode
;;==============================================================================
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md.html\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;==============================================================================
;; adoc-mode
;;==============================================================================
(use-package adoc-mode
  :mode (("\\.adoc\\'" . adoc-mode)))

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

;; No spell check for embedded snippets
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

;; Some key re-mappings
(add-hook 'org-mode-hook
		  (lambda ()
			;; Make meta up/down behave as in all other modes (as per an earlier global setting)
			;; and re-assign org-metadown/-metaup to control meta down/up keys
			(local-set-key [(meta up)] (lambda () (interactive) (scroll-down 4)))
			(local-set-key [(meta down)] (lambda () (interactive) (scroll-up 4)))
			(local-set-key [(control meta down)] 'org-metadown)
			(local-set-key [(control meta up)] 'org-metaup)
			;; ' does not work well with international keyboard layout
			(local-set-key (kbd "C-c ;") 'org-edit-special)))

(add-hook 'org-src-mode-hook
		  (lambda ()
			;; ' does not work well with international keyboard layout
			(local-set-key (kbd "C-c ;") 'org-edit-src-exit)))

;;==============================================================================
;; magit
;;==============================================================================
(use-package magit
  :after (ivy)
  :bind (("C-x g" . magit-status)
		 ("C-x M-g" . magit-dispatch-popup))
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

;;==============================================================================
;; delete-selection-mode
;;==============================================================================
;; For some reason "delete-selection-mode" gets disabled again under Linux
;; if placed near the top of the file
(delete-selection-mode t)

;;==============================================================================
;; Stuff appended by emacs that we don't really want
;;==============================================================================
