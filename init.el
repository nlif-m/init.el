;;; package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(let ((normal-gc-cons-threshold (* 512 1024 1024)) ;; 512 MB
      (init-gc-cons-threshold most-positive-fixnum)) 
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
			(lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))


(setq message-log-max t)
(setq package-enable-at-startup t)
(setq package--init-file-ensured t)
(require 'package)
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))
(add-to-list 'package-archives '("org"   . "https://orgmode.org/elpa/")      t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")    t)
(add-to-list 'package-archives '("elpa"  . "https://elpa.gnu.org/packages/") t)

(mapc #'(lambda (add) (add-to-list 'load-path add))
      (eval-when-compile
		(package-initialize)
		;; Install use-package if not installed yet.
		(unless (package-installed-p 'use-package)
		  (package-refresh-contents)
		  (package-install 'use-package))
		(require 'use-package-ensure)
		(setq use-package-always-ensure t)
		;; (setq use-package-always-defer t)
		(setq use-package-verbose t
			  native-comp-async-report-warnings-errors nil)
		(let ((package-user-dir-real (file-truename package-user-dir)))
		  ;; The reverse is necessary, because outside we mapc
		  ;; add-to-list element-by-element, which reverses.
		  (nreverse (apply #'nconc
						   ;; Only keep package.el provided loadpaths.
						   (mapcar #'(lambda (path)
									   (if (string-prefix-p package-user-dir-real path)
										   (list path)
										 nil))
								   load-path))))))

(setq initial-scratch-message ";; ready to work\n\n")

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(setq gc-cons-percentage 0.2) ;;
(setq read-process-output-max (* 1024 1024)) ;; 1 MB

(setq view-diary-entries-initially t
      mark-diary-entries-in-calendar t
      number-of-diary-entries 7)
(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

(setq display-time-24hr-format 0) ;; 24-часовой временной формат в mode-line
(display-time-mode             0) ;; показывать часы в mode-line
(size-indication-mode          0) ;; размер файла в %-ах

(column-number-mode t)
(setq vc-follow-symlinks t)
(global-display-line-numbers-mode t)
(menu-bar--display-line-numbers-mode-relative)
(setq frame-title-format "GNU Emacs: %b")
(show-paren-mode t)
(setq show-paren-style 'mixed) ;; выделить цветом выражения между {},[],()
(setq require-final-newline    t) ;; добавить новую пустую строку в конец файла при сохранении
;; Clipboard settings

(setq select-enable-clipboard t)

(setq inhibit-startup-screen t)
(tooltip-mode      -1)
(menu-bar-mode     -1) ;; отключаем графическое меню
(tool-bar-mode     -1) ;; отключаем tool-bar
(scroll-bar-mode   -1) ;; отключаем полосу прокрутки
(blink-cursor-mode -1) ;; курсор не мигает
(setq use-dialog-box     nil) ;; никаких графических диалогов и окон - все через минибуфер
(setq ring-bell-function 'ignore) ;; отключить звуковой сигнал

(defvar efs/default-font-size 160)
(defvar efs/default-variable-font-size 180)
(defvar nlif-m/font "freemono")

(set-face-attribute 'default nil :font nlif-m/font :height efs/default-font-size)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font nlif-m/font :height efs/default-font-size)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font nlif-m/font :height efs/default-variable-font-size :weight 'regular)

(setq make-backup-files        nil)
(setq auto-save-default        nil)
(setq auto-save-list-file-name nil) ;; я так привык... хотите включить - замените nil на t
(setq auto-revert-interval 1.1)
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)

(set-language-environment 'UTF-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq-default coding-system-for-read    'utf-8)
(setq file-name-coding-system           'utf-8)
(set-selection-coding-system            'utf-8)
(set-keyboard-coding-system        'utf-8-unix)
(set-terminal-coding-system             'utf-8)
(prefer-coding-system                   'utf-8)

(setq search-highlight        t)
(setq query-replace-highlight t)

(setq scroll-step               1) ;; вверх-вниз по 1 строке
(setq scroll-margin            10) ;; сдвигать буфер верх/вниз когда курсор в 10 шагах от верхней/нижней границы
(setq scroll-conservatively 10000)

(delete-selection-mode t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package async
  :demand t
  :init
  (async-bytecomp-package-mode 1)
  (dired-async-mode 1))

(use-package ibuffer
  :ensure nil
  :bind (("C-x C-b" . ibuffer)))

(require 'font-lock)
(global-font-lock-mode             t) ;; включено с версии Emacs-22. На всякий...
(setq font-lock-maximum-decoration t)

(setq use-package-always-defer t)

(use-package electric
  :ensure nil
  :init
  (electric-pair-mode 1)
  (electric-indent-mode 1)
  )

(use-package semantic
  :ensure nil
  :init
  (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
  (semantic-mode 1)
  )

(use-package ede
  :ensure nil
  :init
  (global-ede-mode t)
  (ede-enable-generic-projects)
  )

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  (setq vertico-scroll-margin 2)

  ;; Show more candidates
  (setq vertico-count 10)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  (defun vertico-resize--minibuffer ()
    (add-hook 'window-size-change-functions
			  (lambda (win)
				(let ((height (window-height win)))
				  (when (/= (1- height) vertico-count)
					(setq-local vertico-count (1- height))
					(vertico--exhibit))))
			  t t))

  (advice-add #'vertico--setup :before #'vertico-resize--minibuffer)
  )

;; Optionally use the `orderless' completion style.
(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless-fast partial-completion basic))
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
		completion-category-defaults nil
		completion-category-overrides '((file (styles partial-completion))))

  :config
  (defun orderless-fast-dispatch (word index total)
    (and (= index 0) (= total 1) (length< word 2)
		 `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

  (orderless-define-completion-style orderless-fast
    (orderless-style-dispatchers '(orderless-fast-dispatch))
    (orderless-matching-styles '(orderless-literal orderless-regexp))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))
;; Enable rich annotations using the Marginalia package

(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (setq text-mode-ispell-word-completion nil)
  )

(use-package corfu
  :demand t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.1)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert) ; insert previewed candidate
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
  :bind
  (:map corfu-map
		("M-SPC"      . corfu-insert-separator)
		("TAB"        . corfu-next)
		([tab]        . corfu-next)
		("S-TAB"      . corfu-previous)
		([backtab]    . corfu-previous)
		("S-<return>" . corfu-insert)
		("RET"        . nil))
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  :config
  (add-hook 'eshell-mode-hook
			(lambda () (setq-local corfu-quit-at-boundary t
								   corfu-quit-no-match t
								   corfu-auto nil)
			  (corfu-mode))
			nil
			t)
  )

(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("M-+ p" . completion-at-point) ;; capf
         ("M-+ t" . complete-tag)        ;; etags
         ("M-+ d" . cape-dabbrev)        ;; or dabbrev-completion
         ("M-+ h" . cape-history)
         ("M-+ f" . cape-file)
         ("M-+ k" . cape-keyword)
         ("M-+ s" . cape-elisp-symbol)
         ("M-+ e" . cape-elisp-block)
         ("M-+ a" . cape-abbrev)
         ("M-+ l" . cape-line)
         ("M-+ w" . cape-dict)
         ("M-+ :" . cape-emoji)
         ("M-+ \\" . cape-tex)
         ("M-+ _" . cape-tex)
         ("M-+ ^" . cape-tex)
         ("M-+ &" . cape-sgml)
         ("M-+ r" . cape-rfc1345))
  :config
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  )

;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode))

(use-package cmake-mode
  :defer)

(use-package cmake-font-lock
  :hook (cmake-mode . cmake-font-lock-mode))

(use-package company-terraform
  :defer
  :config
  (add-to-list 'company-backends 'company-terraform))

(use-package company-shell
  :defer
  :config
  (add-to-list 'company-backends 'company-shell))

(use-package company-ansible
  :defer
  :after (cape)
  :hook (ansible .
				 (lambda ()
				   (add-to-list
					'completion-at-point-functions
					(cape-company-to-capf 'company-ansible))))
  :config
  (add-to-list 'company-backends 'company-ansible))

(use-package which-key
  :defer 1
  :config (which-key-mode))

(use-package yasnippet
  :defer 5
  :config (yas-global-mode t))

(use-package yasnippet-snippets
  :after (yasnippet))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind (("C-h f"	. helpful-callable)
		 ("C-h F"	. helpful-function)
		 ("C-h v"	. helpful-variable)
		 ("C-h k"	. helpful-key)
		 ("C-c C-d"	. helpful-at-point)
		 ("C-h C"	. helpful-command)))

(use-package drag-stuff
  :bind (("M-<up>" . drag-stuff-up)
		 ("M-<down>" . drag-stuff-down)))

(use-package reverse-im
  :custom
  (reverse-im-input-methods '("russian-computer")) ; put your input-method(s) here
  :init
  (reverse-im-mode t))

(use-package projectile
  :defer
  :custom
  (projectile-project-search-path '("~/Code/" "~/golang/" ))
  :config
  (projectile-mode +1)
  (setq projectile-track-known-projects-automatically t)
  :bind (("<f5>" . projectile-compile-project)
		 :map projectile-mode-map
		 ("s-p" . projectile-command-map)
		 ("C-c p" . projectile-command-map)))

(use-package multiple-cursors
  :bind
  ("C->"		. mc/mark-next-like-this)
  ("C-S-c C-S-c"	. mc/edit-lines)
  ("C-<"		. mc/mark-previous-like-this)
  ("C-c C-<"		. mc/mark-all-like-this))

(use-package iedit
  :defer 5)

(use-package writegood-mode
  :ensure t
  :hook ((markdown-mode nroff-mode org-mode
                        mail-mode
                        git-commit-mode)
         . writegood-mode))

(use-package writeroom-mode
  :ensure t
  :commands (writeroom-mode global-writeroom-mode)
  :init
  (setq writeroom-width 90))

(use-package jinx
  :unless (eq system-type 'android)
  :demand t
  :ensure t
  :custom
  (jinx-languages "en ru")
  :bind ("C-c <deletechar>" . jinx-correct)
  :init
  (global-jinx-mode)
  (add-to-list 'ispell-skip-region-alist '("+begin_src" . "+end_src"))
  (setopt flyspell-use-meta-tab nil))

(use-package flycheck
  :config (global-flycheck-mode))

(use-package yaml-mode
  :defer 5)

(use-package flycheck-yamllint
  :after (yaml-mode flycheck)
  :hook
  (yaml-mode . flycheck-yamllint-setup))

(use-package ansible
  :hook (yaml-mode . ansible)
  :defer 5)

(use-package ansible-doc
  :after (yaml-mode ansible)
  :hook
  (yaml-mode . ansible-doc-mode))

(use-package paredit
  :hook
  (scheme-mode . enable-paredit-mode)
  (lisp-interaction-mode . enable-paredit-mode)
  (emacs-lisp-mode  . enable-paredit-mode))

(use-package flycheck-guile
  :after (flycheck)
  :defer 5)

(use-package dart-mode
  :defer)

(use-package elpy
  :defer
  :custom
  (elpy-test-runner  'elpy-test-pytest-runner)
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  :hook
  (elpy-mode . flycheck-mode)
  (python-ts-mode . elpy-enable)
  (python-mode . elpy-enable))

(use-package jinja2-mode
  :defer 5)
(use-package poetry
  :hook (python-mode . poetry-tracking-mode))

(use-package python-pytest)

(use-package python-black
  :hook (python-mode . python-black-on-save-mode))

(use-package dockerfile-mode
  :defer 5)

(use-package web-mode
  :mode
  ("\\.html?\\'" "\\.erb?\\'"))

(use-package rust-mode
  :defer
  :bind
  (("C-c C-c C-c" . rust-compile)))

(use-package caddyfile-mode
  :defer 5)

(use-package go-mode
  :init
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  :mode
  ("\\.go\\'"))

(use-package lua-mode
  :mode
  ("\\.lua\\'"))

(use-package emmet-mode
  :hook
  ((js-mode html-mode css-mode) . emmet-mode))

(use-package prettier
  :commands (prettier-mode))

(use-package markdown-mode
  :defer 5)

(use-package magit
  :bind
  (("C-x g" . magit-status)))

(use-package forge
  :after (magit))

(use-package undo-tree
  :demand t
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-history-directory-alist  `(("." . ,(concat user-emacs-directory ".undo-tree"))))
  (undo-tree-visualizer-timestamps t)
  :config
  (global-undo-tree-mode t))

(use-package evil
  :after (undo-tree)
  :custom
  (evil-want-integration t) ;; This is optional since it's already set to t by default.
  (evil-want-keybinding nil)
  (evil-undo-system 'undo-tree)
  :init
  (evil-mode 1))

(use-package evil-collection
  :after (evil)
  :init
  (evil-collection-init))

(use-package evil-org
  :after (evil org)
  :init
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-paredit
  :config
  (evil-define-operator evil-paredit-delete
						(beg end type register yank-handler)
						"Delete text from BEG to END with TYPE respecting parenthesis.
Save in REGISTER or in the kill-ring with YANK-HANDLER."
						(interactive "<R><x><y>")
						(evil-paredit-yank beg end type register yank-handler)
						(if (eq type 'block)
							(evil-apply-on-block #'delete-region beg end nil)
						  (delete-region beg end))
						;; place cursor on beginning of line
						(when (and (called-interactively-p 'any)
								   (eq type 'line))
						  (evil-first-non-blank)))
  :hook ((paredit-mode emacs-lisp-mode) .  evil-paredit-mode))

(use-package goto-chg
  :after (evil))

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)    ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :init
  (load-theme 'doom-monokai-classic t)
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme
  ;; all-the-icons fonts must be installed!
  (doom-themes-neotree-config))

(use-package swiper
  :defer
  :bind
  (("C-s" . 'swiper)))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (delete 'yaml treesit-auto-langs)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package ssh-config-mode
  :hook (ssh-config-mode . turn-on-font-lock)
  :mode (("/\\.ssh/config\\(\\.d/.*\\.conf\\)?\\'" . ssh-config-mode)
		 ("/sshd?_config\\(\\.d/.*\\.conf\\)?\\'"  . ssh-config-mode)
		 ("/known_hosts\\'"       . ssh-known-hosts-mode)
		 ("/authorized_keys2?\\'" . ssh-authorized-keys-mode)))

(use-package focus-autosave-mode
  :config
  (focus-autosave-mode 1))

(use-package compile
  :defer t
  :hook ((compilation-filter . ansi-color-compilation-filter))
  ;; Using C-u before recompile acts identical to the M-x compile
  :bind (("C-x C-m" . recompile))
  :config
  (setopt compilation-scroll-output t)
  (setopt compilation-ask-about-save nil)
  (require 'ansi-color)
  ;; Custom compilers
  (defun generic-compiler ()
    (concat "compiler "
            (if buffer-file-name
                (shell-quote-argument buffer-file-name))))

  (defun run-on-file (cmd)
    `(lambda () (concat ,cmd " "
                        (shell-quote-argument buffer-file-name))))

  (defvar custom-compiler-modes
    `((purescript-mode . "spago run")
      (vue-ts-mode    . "npx eslint --fix . && npx vue-tsc --noEmit")))

  (defun get-compiler ()
    (let* ((compiler (assoc-default major-mode
                                    custom-compiler-modes
                                    'eql nil)))
      (cond ((or (file-exists-p "makefile")
                 (file-exists-p "Makefile"))
             "make -k ")
            ((functionp compiler) (funcall compiler))
            ((stringp compiler) compiler)
            (t (funcall #'generic-compiler)))))

  ;; A total hack I realized I could do thanks to M-x compile
  ;; executing `(let ((command (eval compile-command))) ...)'
  (setq-default compile-command '(get-compiler))

  ;; Auto focus compilation buffer
  (add-hook 'compilation-finish-functions 'finish-focus-comp)
  (add-hook 'compilation-start-hook 'finish-focus-comp)

  (defun finish-focus-comp (&optional buf-or-proc arg2)
    (let* ((comp-buf (if (processp buf-or-proc)
                         (process-buffer buf-or-proc)
                       buf-or-proc))
           (window (get-buffer-window comp-buf)))
      (if window
          (select-window window)
        (switch-to-buffer-other-window comp-buf)))))

(defun disable-tabs ()
  "It's simple just disable tabs in the file."
  (indent-tabs-mode -1))

(add-hook 'web-mode-hook 'disable-tabs)
(add-hook 'js-ts-mode-hook 'disable-tabs)
(add-hook 'vue-ts-mode-hook 'disable-tabs)
(add-hook 'tsx-ts-mode-hook 'disable-tabs)
(add-hook 'typescript-ts-mode 'disable-tabs)

(use-package eldoc
  :defer 10
  :init
  (setopt eldoc-echo-area-display-truncation-message t)
  (setopt eldoc-echo-area-use-multiline-p nil)
  ;; Make sure Eldoc will show us all of the feedback at point.
  ;; no more clobbering
  (setopt eldoc-documentation-strategy #'eldoc-documentation-compose)
  (global-eldoc-mode t))

(use-package dired
  :ensure nil
  :defer
  :commands (dired)
  :hook ((dired-mode . hl-line-mode)
         (dired-mode . dired-hide-details-mode))
  :bind (:map dired-mode-map
              ("-" . dired-up-directory))
  :init
  ;; let me drag files into other programs
  (setq dired-mouse-drag-files t)
  (setq dired-bind-jump nil)
  :config
  (setq dired-listing-switches "-agho --group-directories-first")
;;;;; xdg-open integration
  (require 'dired-x)
  ;; prevent opening extra dired buffers
  ;; emacs 28
  (setq dired-kill-when-opening-new-dired-buffer t))

(use-package docker
  :ensure t
  :bind (("C-c d" . docker)))

(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
		  ansible-doc-mode
		  rst-mode
		  helpful-mode
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package org
  :ensure t
  :defer 5
  :hook
  (org-agenda-mode . (lambda () (hl-line-mode 1)))
  (org-mode . visual-line-mode)
  (org-mode . (lambda () (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))))
  :bind
  (("C-c n y" . org-id-get-create)
   ("C-c l"   . org-store-link)
   ("C-c a"   . org-agenda)
   ("<f12>" . org-agenda)
   ("C-c c"   . org-capture))
  :config
  ;; hl-line
  (require 'hl-line)
  (set-face-background 'hl-line "red")
  ;; end hl-line
  (setq org-directory "~/org")
  (setq org-default-notes-file "~/org/inbox.org")
  
  (setq org-catch-invisible-edits 'smart)
  (setq org-return-follows-link t)
  (set-inbox-buffers)
  (setq org-capture-templates
		(quote (("t" "Todo [inbox]" entry (file+headline "~/org/inbox.org" "inbox")
				 "* TODO %i%?")
				("е" "Todo [inbox]" entry (file+headline "~/org/inbox.org" "inbox")
				 "* TODO %i%?")
				("j" "Journal" entry (file+datetree "~/org/diary.org")
				 "* %?\n%U\n"))))

  ;; Do not dim blocked tasks
  (setq org-agenda-dim-blocked-tasks nil)
  ;; Compact the block agenda view
  (setq org-agenda-compact-blocks t)
  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

  (when termux-p
    (add-to-list 'org-file-apps '("\\.pdf\\'" . "termux-open %s"))
    (add-to-list 'org-file-apps '("\\.png\\'" . "termux-open %s"))
    (add-to-list 'org-file-apps '("\\.jpg\\'" . "termux-open %s"))
    (add-to-list 'org-file-apps '("\\.jpeg\\'" . "termux-open %s")))

  ;; Custom agenda command definitions
  (setq org-agenda-custom-commands
		(quote (
				("i" "inbox" tags-todo "inbox"
				 ((org-agenda-overriding-header "inbox")))
				("ш" "inbox" tags-todo "inbox"
				 ((org-agenda-overriding-header "inbox")))
				(" " "Текущие дела" tags-todo "todo"
				 ((org-agenda-overriding-header "Текущие дела")))
				)))
  (setq org-use-fast-todo-selection t)	; C-c C-t
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)
  (setq org-refile-targets
		(quote (
				("~/org/todo.org"		:maxlevel . 3)
				("~/org/calendar.org"	:maxlevel . 2)
				("~/org/waiting.org"	        :maxlevel . 2)
				("~/org/someday.org"		:maxlevel . 2)
				("~/org/inbox.org"		:maxlevel . 1)
				("~/org/garbage.org"		:maxlevel . 1)
				("~/org/projects.org"        :maxlevel . 2))))

										; Use full outline paths for refile targets - we file directly with IDO
  (setq org-refile-use-outline-path t)

										; Targets complete directly with IDO
  (setq org-outline-path-complete-in-steps nil)

										; Allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes (quote confirm))

										; Use IDO for both buffer and file completion and ido-everywhere to t
  (setq org-completion-use-ido t)
  (setq org-indirect-buffer-display 'current-window)

;;;; Refile settings
										; Exclude DONE state tasks from refile targets
  (defun bh/verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))

  (setq org-refile-target-verify-function 'bh/verify-refile-target)
  (run-at-time "00:59" 3600 'org-save-all-org-buffers)
  (setq org-edit-src-content-indentation 0)
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((latex . t)
     (emacs-lisp . t)
     (lisp . t)
     (lua . t)
     (shell . t)
     (scheme . t)
     (octave . t)
     (python . t)))
  (font-lock-add-keywords
   'org-mode
   '(("^ *\\([-]\\) "
      (0 (prog1 () (compose-region
					(match-beginning 1) (match-end 1) "•")))))))


(use-package org-roam
  :commands (org-roam-buffer-toggle org-roam-node-find org-roam-graph org-roam-capture org-roam-node-insert org-roam-tag-add org-roam-alias-add org-roam-ref-add)
  :hook
  (org-mode . org-roam-db-autosync-mode)
  :bind (
		 ("C-c n l" . org-roam-buffer-toggle)
		 ("C-c n f" . org-roam-node-find)
		 ("C-c n g" . org-roam-graph)
		 ("C-c n c" . org-roam-capture)
		 ("C-c n i" . org-roam-node-insert)
		 ("C-c n t" . org-roam-tag-add)
		 ("C-c n a" . org-roam-alias-add)
		 ("C-c n r" . org-roam-ref-add)
		 :map org-mode-map
		 ("C-M-i" . completion-at-point))
  :custom
  (org-roam-v2-ack t)
  (org-roam-capture-templates
   '(
     ("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>.org"
						 "#+title: ${title}\n* COMMENT References                                                      :TODO:\n")
      :unnarrowed t)
     
     ("в" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>.org"
						 "#+title: ${title}\n* COMMENT References                                                      :TODO:\n")
      :unnarrowed t)
     ))
  (org-roam-directory (file-truename "~/org-roam"))
  (org-roam-completion-everywhere t)
  :config
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
				 (display-buffer-in-direction)
				 (direction . right)
				 (window-width . 0.33)
				 (window-height . fit-window-to-buffer)))
  )

(use-package org-roam-ui
  :after (org-mode org-roam)
  :defer 2
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil)
  (org-roam-ui-custom-theme
   '((bg . "#1E2029")
     (bg-alt . "#282a36")
     (fg . "#f8f8f2")
     (fg-alt . "#6272a4")
     (red . "#ff5555")
     (orange . "#f1fa8c")
     (yellow ."#ffb86c")
     (green . "#50fa7b")
     (cyan . "#8be9fd")
     (blue . "#ff79c6")
     (violet . "#8be9fd")
     (magenta . "#bd93f9"))))

(use-package org-download
  :after (org)
  :hook
  (dired-mode . org-download-enable)
  :custom
  (org-download-image-dir "~/org/files/images/")
  :bind
  (:map org-mode-map
        (("C-c n s s" . org-download-screenshot)
		 ("C-c n s d" . org-download-delete)
         ("C-c n s y" . org-download-yank))))

(use-package org-modern
  :hook (org-mode . org-modern-mode))

(use-package eglot
  :ensure nil
  :hook ((go-mode python-mode) . (lambda () (eglot-ensure) (flycheck-eglot-mode) (company-mode 0)))
  :bind (:map eglot-mode-map
			  ("C-c r" . eglot-rename)
			  ("C-c o" . eglot-code-action-organize-imports)
			  ("C-c h" . eldoc)
			  )
  :custom
  (eglot-autoshutdown t)
  (eglot-inlay-hints-mode t)
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref nil)
  (eldoc-echo-area-use-multiline-p 5)
  (eglot-stay-out-of '(yasnippet))
  (eglot-ignored-server-capabilities
   '(:hoverProvider
     :documentHighlightProvider
     :documentFormattingProvider
     :documentRangeFormattingProvider
     :documentOnTypeFormattingProvider
     :colorProvider
     :foldingRangeProvider))
  :config
  (setq-default
   eglot-workspace-configuration
   '((:gopls .
			 (
			  ;; (gofumpt . t)
			  (usePlaceholders . t)
			  ;; (staticcheck . t)
			  (hints . (
						(assignVariableTypes . t)
						(compositeLiteralFields . t)
						(compositeLiteralTypes . t)
						(constantValues . t)
						(functionTypeParameters . t)
						(parameterNames . t)
						(rangeVariableTypes . t)))
			  ))))
  
  (require 'project)

  (defun project-find-go-module (dir)
	(when-let ((root (locate-dominating-file dir "go.mod")))
      (cons 'go-module root)))

  (cl-defmethod project-root ((project (head go-module)))
	(cdr project))

  (add-hook 'project-find-functions #'project-find-go-module)
  )

(use-package flycheck-eglot
  :after (flycheck eglot)
  :init
  (global-flycheck-eglot-mode 1))


(provide 'init)
;;; init.el ends here
