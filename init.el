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
      '((bind-key           . "melpa")
        (diminish           . "melpa")
        (use-package        . "melpa")))

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
;; PATH MANIPULATION

;; Setting the `load-path` allows the rest of my configuration to be
;; broken into separate modules.

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; Adjusting the `exec-path` is necessary because OSX desktop launch
;; doesn't happen in a shell and therefore doesn't get the path
;; configured in your shell.  For my purposes I only need
;; /usr/local/bin. While we're at it, we can also make sure $PATH is
;; setup properly in the environment for `eshell`.
(add-to-list 'exec-path "/usr/local/bin")
(setenv "PATH" (mapconcat 'identity exec-path ":"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOMIZATIONS

(load "neo-wip")
(load "neo-general")
(load "neo-dev-general")
(load "neo-dev-lisp")
(load "neo-games")
(load "neo-messaging")
(load "neo-navigation")
(load "neo-org")
(load "neo-shell")
(load "neo-ux")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (2048-game zenburn-theme virtualenvwrapper use-package smart-mode-line rainbow-delimiters projectile powerline paredit package-lint origami magithub highlight-symbol helm-ag header2 gnugo docker diminish dashboard company cider better-defaults alert adafruit-wisdom))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
