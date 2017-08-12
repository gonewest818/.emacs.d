(require 'package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE MANAGEMENT

;; set up package archives
(add-to-list 'package-archives          ; melpa stable
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; marmalade repo
(add-to-list 'package-archives          ; marmalade
             '("marmalade" . "https://marmalade-repo.org/packages/") t)

;; manual initialization, see Stack Overflow [http://bit.ly/2tu5N8s] 
(setq package-enable-at-startup nil)
(package-initialize)

;; refresh package archive contents only if it's empty
;; if you need to update packages, do it manually
(when (not package-archive-contents)
    (package-refresh-contents))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLOJURE / CIDER CONFIGURATION

;; selection of packages for clojure work
(defvar my-packages '(better-defaults
                      cider
                      clojure-mode
                      company
                      highlight-symbol
                      magit
                      markdown-mode
                      paredit
                      projectile
                      rainbow-delimiters
                      zenburn-theme))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;; Set the default comment column to 60
(setq-default comment-column 60)

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

;; vertical alignment
(setq clojure-align-forms-automatically 't)

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
;; Other packages, not related to clojure development

;; adjust exec-path as necessary because OSX window system launch
;; doesn't respect your shell config
(add-to-list 'exec-path "/usr/local/bin")
;; make sure PATH also matches, for eshell
(setenv "PATH" (mapconcat 'identity exec-path ":"))

;; org-mode export to markdown 
(eval-after-load "org"
  '(require 'ox-md nil t))

;; ERC mode channel and server configuration

(load "~/.emacs.d/.erc-auth")

(setq erc-nickserv-passwords
      `((freenode    (("gonewest"    . ,my-freenode-pass)
                      ("gonewest818" . ,my-freenode-pass)))
        (QuakeNet    (("gonewest818" . ,my-quakenet-pass)))))

(setq erc-autojoin-channels-alist '(("freenode" "#emacs"
                                                "#leiningen"
                                                "#datomic")
                                    ("QuakeNet" "#overwatch")))

(require 'erc-services)
(erc-services-mode 1)
(setq erc-prompt-for-nickserv-password nil)

;; ERC track mode
(require 'erc-track)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))
(setq erc-track-exclude-server-buffer t)
(erc-track-mode 1)

;; hotkeys to join servers
(global-set-key "\C-cef" (lambda () (interactive)
                           (erc :server "irc.freenode.net" :port "6667"
                                :nick "gonewest818")))

(global-set-key "\C-ceq" (lambda () (interactive)
                           (erc :server "irc.quakenet.org" :port "6667"
                                :nick "gonewest818")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zenburn theme customization
;; refer to theme source code [http://bit.ly/2tI2uGW]

(defvar zenburn-override-colors-alist
  '( ;; uncomment any of the following and change
;    ("zenburn-fg+1"     . "#FFFFEF")
;    ("zenburn-fg"       . "#DCDCCC")
;    ("zenburn-fg-1"     . "#656555")
;    ("zenburn-bg-2"     . "#000000")
;    ("zenburn-bg-1"     . "#2B2B2B")
;    ("zenburn-bg-05"    . "#383838")
;    ("zenburn-bg"       . "#3F3F3F")
;    ("zenburn-bg+05"    . "#494949")
;    ("zenburn-bg+1"     . "#4F4F4F")
;    ("zenburn-bg+2"     . "#5F5F5F")
;    ("zenburn-bg+3"     . "#6F6F6F")
;    ("zenburn-red+1"    . "#DCA3A3")
;    ("zenburn-red"      . "#CC9393")
;    ("zenburn-red-1"    . "#BC8383")
;    ("zenburn-red-2"    . "#AC7373")
;    ("zenburn-red-3"    . "#9C6363")
;    ("zenburn-red-4"    . "#8C5353")
;    ("zenburn-orange"   . "#DFAF8F")
;    ("zenburn-yellow"   . "#F0DFAF")
;    ("zenburn-yellow-1" . "#E0CF9F")
;    ("zenburn-yellow-2" . "#D0BF8F")
;    ("zenburn-green-1"  . "#5F7F5F")
;    ("zenburn-green"    . "#7F9F7F")
;    ("zenburn-green+1"  . "#8FB28F")
;    ("zenburn-green+2"  . "#9FC59F")
;    ("zenburn-green+3"  . "#AFD8AF")
;    ("zenburn-green+4"  . "#BFEBBF")
;    ("zenburn-cyan"     . "#93E0E3")
;    ("zenburn-blue+1"   . "#94BFF3")
;    ("zenburn-blue"     . "#8CD0D3")
;    ("zenburn-blue-1"   . "#7CB8BB")
;    ("zenburn-blue-2"   . "#6CA0A3")
;    ("zenburn-blue-3"   . "#5C888B")
;    ("zenburn-blue-4"   . "#4C7073")
;    ("zenburn-blue-5"   . "#366060")
;    ("zenburn-magenta"  . "#DC8CC3")
;; end of list
  ))

(defun nokamoto-customize-zenburn ()
  (zenburn-with-color-variables
   (custom-theme-set-faces
    'zenburn
    `(fringe ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))
    `(cider-fringe-good-face ((t (:foreground ,zenburn-green+1))))
    )))

;; Set theme
(load-theme 'zenburn t)
(nokamoto-customize-zenburn)

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

(global-set-key [f9]            'cider-jack-in)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-x g")   'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(setq company-minimum-prefix-length 2)
(define-key company-active-map (kbd "<tab>")   'company-complete-common-or-cycle)
(define-key company-active-map (kbd "S-<tab>") 'company-select-previous)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; liberally copied from
;; http://fgiasson.com/blog/index.php/2016/06/14/my-optimal-gnu-emacs-settings-for-developing-clojure-revised/
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(custom-safe-themes
   (quote
    ("bfdcbf0d33f3376a956707e746d10f3ef2d8d9caa1c214361c9c08f00a1c8409" default)))
 '(fci-rule-color "#383838")
 '(inhibit-startup-screen t)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (magit markdown-mode rainbow-delimiters projectile monokai-theme highlight-symbol helm-ag company cider better-defaults)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
