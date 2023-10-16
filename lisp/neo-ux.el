;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; USER INTERACTION AND DESIGN

(use-package dimmer
  :if (file-exists-p "~/.emacs.d/dev/dimmer.el/")
  :load-path "~/.emacs.d/dev/dimmer.el/"
  :config
  (setq dimmer-fraction 0.30)
  (setq dimmer-adjustment-mode :foreground)
  (dimmer-configure-which-key)
  (dimmer-configure-org)
  (dimmer-mode t))

(use-package dimmer
  :if (not (file-exists-p "~/.emacs.d/dev/dimmer.el/"))
  :ensure t
  :config
  (setq dimmer-fraction 0.30)
  (setq dimmer-adjustment-mode :foreground)
  (dimmer-configure-which-key)
  (dimmer-configure-org)
  (dimmer-mode t))

(use-package highlight-symbol
  :ensure t
  :pin melpa-stable
  :bind (("C-<f3>" . highlight-symbol)
         (  "<f3>" . highlight-symbol-next)
         ("S-<f3>" . highlight-symbol-prev)
         ("M-<f3>" . highlight-symbol-query-replace)))

(use-package smart-mode-line
  :ensure t
  :pin melpa-stable
  :hook (after-init . sml/setup)
  :init
  (setq sml/battery-format " %b%p%%")
  (setq display-time-format "%l:%M%#p") ; e.g. 4:48pm
  (setq display-time-default-load-average 0)
  (setq sml/use-projectile-p 'before-prefixes)
  (setq sml/shorten-directory t)
  (setq sml/shorten-modes t)
  (setq sml/name-width 40)
  (setq sml/mode-width 'full)
  :config
  (line-number-mode 1)
  (column-number-mode 1)
  (display-time-mode 1)
  (display-battery-mode 1))

(use-package zenburn-theme
  :ensure t
  :pin melpa-stable
  :init
  (load-theme 'zenburn t)
  (zenburn-with-color-variables
    (custom-theme-set-faces
     'zenburn
     ;; prefer the fringe background to be same as bg
     `(fringe ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))
     ;; make these easier to see
     `(cider-fringe-good-face ((t (:foreground ,zenburn-green+1))))
     ;; don't invert the git-gutter faces
     `(git-gutter:added ((t (:foreground ,zenburn-green :weight bold :inherit default))))
     `(git-gutter:deleted ((t (:foreground ,zenburn-red :weight bold :inherit default))))
     `(git-gutter:modified ((t (:foreground ,zenburn-magenta :weight bold :inherit default))))
     `(git-gutter:unchanged ((t (:background ,zenburn-yellow-2 :inherit default))))
     ;; bring the eval "flashes" in line with this theme
     `(eval-sexp-fu-flash
       ((t (:background ,zenburn-blue-5 :bold t))))
     `(eval-sexp-fu-flash-error
       ((t (:background ,zenburn-red-3 :bold t))))
     ;; modeline customizations
     `(mode-line
       ((,class (:box (:line-width 1 :color ,zenburn-bg-2)
                      :foreground ,zenburn-green+1
                      :background ,zenburn-bg-1))))
     `(mode-line-inactive
       ((,class (:box (:line-width 1 :color ,zenburn-bg-1)
                      :foreground ,zenburn-green-1
                      :background ,zenburn-bg-05)))))))

(use-package which-key
  :ensure t
  :pin melpa-stable
  :diminish ""
  :config
  (which-key-mode))

(use-package expand-region
  :ensure t
  :pin melpa-stable
  :bind ("C-=" . er/expand-region))

(use-package keycast
  :ensure t)

(setq inhibit-startup-screen t)

(global-hl-line-mode)                   ; highlight current line

;; Configure new frames
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'default-frame-alist '(font . "Inconsolata-12"))

;; Winner mode allows layout undo [C-c left] & redo [C-c right]
(winner-mode t)

(defun neo-terminal-visible-bell ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))

(setq ring-bell-function #'neo-terminal-visible-bell)

;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; TAB navigation
(defun neo-other-window-rev ()
  "other-window with reversed direction for keybinding"
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-<tab>")   'other-window)
(global-set-key (kbd "C-S-<tab>") 'neo-other-window-rev)
