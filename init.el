;;; package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; -*- lexical-binding: t; -*-
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

;; (package-initialize)
;; ;; Install use-package if not installed yet.
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; (require 'use-package)
;; (setq use-package-always-ensure t)

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
	(setq use-package-verbose t)
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

;; (setq gc-cons-threshold (* 512 1024 1024)) ;; 512 MB
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

(load-theme 'wombat)

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

(require  'bs)
(require  'ibuffer)
(defalias 'list-buffers 'ibuffer) ;; отдельный список буферов при нажатии C-x C-b
(global-set-key (kbd "<f2>") 'bs-show) ;; запуск buffer selection кнопкой F2

(require 'font-lock)
(global-font-lock-mode             t) ;; включено с версии Emacs-22. На всякий...
(setq font-lock-maximum-decoration t)

(setq use-package-always-defer t)

(defun nlif-m/dired-in-cwd ()
  "Open `dired` in current working directory"
  (interactive)
  (dired default-directory))
(keymap-global-set "M-p" 'nlif-m/dired-in-cwd)

(defun nlif-m/open-shell ()
  "Open $TERMINAL in current directory."
  (interactive)
  (let ((p  (start-process "terminal" nil (getenv "TERMINAL"))))
    (set-process-sentinel p (lambda (process state)
			      (when (equal state "finished")
				(kill-process process))))))
(global-set-key (kbd "<f7>") 'nlif-m/open-shell)

(setq browse-url-generic-program "sender-go")
(global-set-key (kbd "<f6>") 'browse-url-generic)

(defun nlif-m//trans (start end lang)
  "A generic function to translate region from START to END to provided LANG."
  (let ((text (buffer-substring-no-properties start end))
	(trans-buffer-name "*trans-buffer*"))
    (let ((trans-buffer (get-buffer-create trans-buffer-name)))
      (start-process (concat "trans-" lang) trans-buffer "trans" "-t" lang text))))

(defun nlif-m/trans-toru (start end)
  "Translates text from START to END to russian language."
  (interactive "r")
  (nlif-m//trans start end "ru"))

(defun nlif-m/trans-toen (start end)
  "Translates text from START to END to english language."
  (interactive "r")
  (nlif-m//trans start end "en"))

(require 'electric)
(electric-pair-mode 1)
(electric-indent-mode 1)

(require 'semantic)
(require 'semantic/bovine/gcc)

(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)

(semantic-mode 1)
(global-ede-mode t)
(ede-enable-generic-projects)

(use-package async
  :demand t
  :init
  (async-bytecomp-package-mode 1)
  (dired-async-mode 1))

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
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion))))

  :config
  (defun orderless-fast-dispatch (word index total)
    (and (= index 0) (= total 1) (length< word 3)
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
  (setq text-mode-ispell-word-completion nil))

(use-package corfu
  ;; Optional customizations
  :after (orderless)
  :demand t
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match 'separator) ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 1)
  (completion-styles '(orderless-fast partial-completion basic))
  :init
  (global-corfu-mode))

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

;; (use-package company)

(use-package cmake-mode
  :defer)

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
  :config
  (add-to-list 'company-backends 'company-ansible))

(use-package clang-capf
  :defer)

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
  :custom
  (projectile-project-search-path '("~/Code/" "~/golang/" ))
  :init
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

(use-package guix)

(use-package flycheck
  :config (global-flycheck-mode))

(use-package yaml-mode
  :defer 5)

(use-package flycheck-yamllint
  :after (yaml-mode flycheck)
  :hook
  (yaml-mode . flycheck-yamllint-setup))

(use-package ansible
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
  :hook (dart-mode . flymake-mode-off)
  :config
  (when (require 'flycheck nil t)
    (flycheck-define-checker dart-checker
      " An extremely fast Python linter, written in Rust.

See URL `https://github.com/charliermarsh/ruff`."

      :command ("dart" "analyze" source)
      :error-patterns
      (
       (error line-start (zero-or-more space) "error" " - " (file-name) ":" line ":" column " - " (message) " - " (id (one-or-more not-newline)) line-end)
       
       (warning line-start (zero-or-more space) "warning" " - " (file-name) ":" line ":" column " - " (message) " - " (id (one-or-more not-newline)) line-end)
       )
      :modes dart-mode)

    (add-to-list 'flycheck-checkers 'dart-checker))
  )

;; (use-package elpy
;;   :defer
;;   :custom
;;   (elpy-test-runner  'elpy-test-pytest-runner)
;;   :config
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (when (require 'flycheck nil t)
;;     (flycheck-define-checker python-ruff
;;       " An extremely fast Python linter, written in Rust.

;; See URL `https://github.com/charliermarsh/ruff`."

;;       :command ("python" "-m" "ruff" "check" "-q"  "--stdin-filename" source "-")
;;       :standard-input t
;;       :next-checkers ((warning . python-mypy))
;;       :error-patterns
;;       ((error line-start
;; 	      (file-name) ":" line ":" column ": " (id)  (message)
;; 	      line-end))
;;       :modes python-mode)

;;     (add-to-list 'flycheck-checkers 'python-ruff))
;;   :hook
;;   (elpy-mode . flycheck-mode)
;;   (python-ts-mode . elpy-enable)
;;   (python-mode . elpy-enable))

(use-package jinja2-mode
  :defer 5)
(use-package poetry
  :hook (python-mode . poetry-tracking-mode))

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

(use-package eglot
  :defer
  :custom
  (eldoc-echo-area-use-multiline-p t)
  :bind
  (
   (:map eglot-mode-map
	 ("C-c <tab>" .  #'company-complete) ; initiate the completion manually
	 ;; ("C-c e f n" .  #'flymake-goto-next-error)
	 ;; ("C-c e f p" .  #'flymake-goto-prev-error)
	 ("C-c e r" .  #'eglot-rename))))

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
  :hook ((paredit-mode emacs-lisp-mode) .  evil-paredit-mode))
(use-package goto-chg
  :after (evil))

(use-package doom-themes
  :if window-system
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

(use-package ssh-config-mode
  :hook (ssh-config-mode . turn-on-font-lock)
  :mode (("/\\.ssh/config\\(\\.d/.*\\.conf\\)?\\'" . ssh-config-mode)
	 ("/sshd?_config\\(\\.d/.*\\.conf\\)?\\'"  . ssh-config-mode)
	 ("/known_hosts\\'"       . ssh-known-hosts-mode)
	 ("/authorized_keys2?\\'" . ssh-authorized-keys-mode)))

(use-package focus-autosave-mode
  :config
  (focus-autosave-mode 1))

(provide 'init)
;;; init.el ends here
