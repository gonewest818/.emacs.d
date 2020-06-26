(require 'package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE INITIALIZATION

;; Set up package archives
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(add-to-list 'package-archives
             '("melpa"        . "https://melpa.org/packages/") t)

;; See Stack Overflow [http://bit.ly/2tu5N8s] 
(setq package-enable-at-startup nil)
(package-initialize)

;; Packages with preferred repo
(setq package-pinned-packages
      '((bind-key             . "melpa")
        (diminish             . "melpa")
        (exec-path-from-shell . "melpa")
        (no-littering         . "melpa")
        (use-package          . "melpa")))

;; Refresh package archive contents only if it's empty.
;; If you need to update packages, do it manually.
(when (not package-archive-contents)
    (package-refresh-contents))

(dolist (p (mapcar 'car package-pinned-packages))
  (unless (package-installed-p p)
    (package-install p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; USE-PACKAGE INITIALIZATION

(eval-when-compile
  (require 'use-package))
(setq  use-package-compute-statistics t)
(require 'diminish)
(require 'bind-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NO-LITTERING (https://github.com/emacscollective/no-littering)

(require 'no-littering)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM

(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(when (file-readable-p custom-file) (load custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PATH MANIPULATION

;; Setting the `load-path` allows the rest of my configuration to be
;; broken into separate modules.

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; Adjusting the `exec-path` is necessary because OSX desktop launch
;; doesn't happen in a shell and therefore doesn't get the path
;; configured in your shell.

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AUTH-SOURCE AND GNUPG

(setq auth-source-debug t)
(require 'auth-source-pass)
(auth-source-pass-enable)
;;(setq auth-sources '((:source "~/.authinfo.gpg")))
(setq epa-pinentry-mode 'loopback)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOMIZATIONS

(load "neo-general")
(load "neo-browsing")
(load "neo-dev-general")
(load "neo-dev-lisp")
(load "neo-dev-python")
(load "neo-games")
(load "neo-messaging")
(load "neo-navigation")
(load "neo-org")
(load "neo-shell")
(load "neo-ux")
(load "neo-writing")
(load "neo-wip")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
