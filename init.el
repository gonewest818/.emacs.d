(require 'package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE MANAGEMENT

;; set up package archives
(add-to-list 'package-archives          ; melpa stable
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;;(add-to-list 'package-archives        ; melpa latest
;;             '("melpa" . "http://melpa.org/packages/") t)

;; marmalade repo
(add-to-list 'package-archives          ; marmalade
             '("marmalade" . "https://marmalade-repo.org/packages/") t)

(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLOJURE / CIDER CONFIGURATION

;; selection of packages for clojure work
(defvar my-packages '(better-defaults
                      projectile
                      clojure-mode
                      cider
                      company
                      paredit
                      monokai-theme
                      rainbow-delimiters
                      highlight-symbol))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;; Set the default comment column to 60
(setq-default comment-column 60)

;; adjust so that lein is in the path
(add-to-list 'exec-path "/usr/local/bin")

;; parinfer configuration
;(setq parinfer-extensions '(defaults pretty-parens smart-tab smart-yank))
;(global-set-key [?\C-,] 'parinfer-toggle-mode)
;(add-hook 'clojure-mode-hook #'parinfer-mode)

;; paredit configuration
(add-hook 'clojure-mode-hook #'paredit-mode)

;; Enter cider mode when entering the clojure major mode
(add-hook 'clojure-mode-hook #'cider-mode)

;; Turn on auto-completion with Company-Mode
(global-company-mode)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

;; Replace return key with newline-and-indent when in cider mode.
(add-hook 'cider-mode-hook '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))

;; Show parenthesis mode
(show-paren-mode 1)

;; rainbow delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; custom clojure-mode indentation
(require 'clojure-mode)
(define-clojure-indent
  ;; midje
  (fact 'defun)
  (facts 'defun)
  (fact-group 'defun)
  (silent-fact 'defun)
  (future-fact 'defun)
  (tabular 'defun)
  (against-background 'defun)
  (provided 0)
  ;; compojure
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company mode adjustments for Monokai theme

;;
;; Update the color of the company-mode context menu to fit the Monokai theme
;; @source: https://github.com/search?q=deftheme+company-tooltip&type=Code
;;
(deftheme monokai-overrides)

(let ((class '((class color) (min-colors 257)))
      (terminal-class '((class color) (min-colors 89))))

  (custom-theme-set-faces
   'monokai-overrides

   ;; Linum and mode-line improvements (only in sRGB).
   `(linum
     ((,class :foreground "#75715E"
              :background "#49483E")))
   `(mode-line-inactive
     ((,class (:box (:line-width 1 :color "#2c2d26" :style nil)
                    :background "#2c2d26"))))

   ;; Custom region colouring.
   `(region
     ((,class :foreground "#75715E"
              :background "#49483E")
      (,terminal-class :foreground "#1B1E1C"
                       :background "#8B8878")))

   ;; Additional modes
   ;; Company tweaks.
   `(company-tooltip-common
     ((t :foreground "#F8F8F0"
         :background "#474747"
         :underline t)))

   `(company-template-field
     ((t :inherit company-tooltip
         :foreground "#C2A1FF")))

   `(company-tooltip-selection
     ((t :background "#349B8D"
         :foreground "#BBF7EF")))

   `(company-tooltip-common-selection
     ((t :foreground "#F8F8F0"
         :background "#474747"
         :underline t)))

   `(company-scrollbar-fg
     ((t :background "#BBF7EF")))

   `(company-tooltip-annotation
     ((t :inherit company-tooltip
         :foreground "#C2A1FF")))

   ;; Popup menu tweaks.
   `(popup-menu-face
     ((t :foreground "#A1EFE4"
         :background "#49483E")))

   `(popup-menu-selection-face
     ((t :background "#349B8D"
         :foreground "#BBF7EF")))

   ;; Circe
   `(circe-prompt-face
     ((t (:foreground "#C2A1FF" :weight bold))))

   `(circe-server-face
     ((t (:foreground "#75715E"))))

   `(circe-highlight-nick-face
     ((t (:foreground "#AE81FF" :weight bold))))

   `(circe-my-message-face
     ((t (:foreground "#E6DB74"))))

   `(circe-originator-face
     ((t (:weight bold))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI ADJUSTMENTS

;; Set theme & font size
(add-to-list 'custom-theme-load-path "~/.emacs.d/lib/monokai-theme")
(load-theme 'monokai t)

;; UTF-8 as default encoding
(set-language-environment "UTF-8")

;; my favorite font
(set-default-font "Inconsolata-12")

;; Every time a window is started, make sure it get maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; customize the visible bell to flash only the mode line
(setq visible-bell nil)

(defun my-terminal-visible-bell ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))

(setq ring-bell-function #'my-terminal-visible-bell)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYBOARD BINDING

(global-set-key [f9] 'cider-jack-in)
;;(global-set-key [apps] 'other-frame)
(global-set-key [f11] 'speedbar)

(setq company-minimum-prefix-length 2)
(define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "S-<tab>") 'company-select-previous)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; liberally copied from
;; http://fgiasson.com/blog/index.php/2016/06/14/my-optimal-gnu-emacs-settings-for-developing-clojure-revised/
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (rainbow-delimiters projectile monokai-theme highlight-symbol helm-ag company cider better-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
