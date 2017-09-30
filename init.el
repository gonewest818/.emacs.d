(require 'package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE MANAGEMENT

;; set up package archives
(add-to-list 'package-archives          ; melpa stable
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

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

(defvar my-packages '(better-defaults
                      cider
                      clojure-mode
                      company
                      gnugo
                      helm-ag
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLOJURE / CIDER CONFIGURATION

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

;; rainbow delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Show parenthesis mode
(show-paren-mode 1)

;; Set the default comment column to 60
(setq-default comment-column 60)

;; pretty printing (via fipp, the default)
(setq cider-repl-use-pretty-printing 't)
(setq cider-pprint-fn 'fipp)

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

;; adjust exec-path
;; necessary because OSX window system launch doesn't use your shell config
;; also make sure PATH also matches, for eshell
(add-to-list 'exec-path "/usr/local/bin")
(setenv "PATH" (mapconcat 'identity exec-path ":"))

;; Print settings
;; When changing the font size you also need to adjust the pagination
;; as follows:
;;   if "lpr-headers-switches" is non-nil, the flags are passed directly to lpr
;;   if "lpr-headers-switches" is nil, then pagination is done by pr
;;   you can pass flags to pr by setting "lpr-page-header-switches"
(setq lpr-switches '("-o" "cpi=14"          ; characters per inch
                     "-o" "lpi=8"           ; lines per inch
                     "-o" "page-top=32"     ; margins in points (1/72 inch)
                     "-o" "page-bottom=32"
                     "-o" "page-left=32"
                     "-o" "page-right=32"))
(setq lpr-page-header-switches '("-l" "80") ; lines per page

;; org-mode export to markdown 
(eval-after-load "org"
  '(require 'ox-md nil t))

;; ERC mode channel and server configuration

(load "~/.emacs.d/.erc-auth")

(setq erc-nickserv-passwords
      `((freenode    (("gonewest"    . ,my-freenode-pass)
                      ("gonewest818" . ,my-freenode-pass)))
        (QuakeNet    (("gonewest818" . ,my-quakenet-pass)))))

(setq erc-autojoin-channels-alist '(("freenode" "#clojure"
                                                "#clojurescript"
                                                "#clojure-beginners"
                                                "#clojure-emacs"
                                                "#emacs"
                                                "#datomic"
                                                "#leiningen")
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


;; GnuGo newbie setup
(setq gnugo-option-history (list "--komi 5.5 --boardsize 13"))
(setq gnugo-xpms 'gnugo-imgen-create-xpms)
(add-hook 'gnugo-start-game-hook 'gnugo-image-display-mode)


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

(setq inhibit-startup-screen t)

;; UTF-8 as default encoding
(set-language-environment "UTF-8")

;; favorite font
(set-default-font "Inconsolata-12")
;;(set-default-font "Iosevka-12")

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
