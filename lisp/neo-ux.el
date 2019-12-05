;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; USER INTERACTION AND DESIGN

(if (setq nokamoto-dimmer-use-dev t)    ; set true to use dev version
    (use-package dimmer
      :load-path "~/.emacs.d/dev/dimmer.el/"
      :config
      (setq dimmer-fraction 0.1)
      (setq dimmer-adjustment-mode :both)
      (dimmer-configure-which-key)
      (dimmer-configure-helm)
      (dimmer-mode t))
  (use-package dimmer
    :ensure t
    :config
    (setq dimmer-fraction 0.1)
    (setq dimmer-adjustment-mode :both)
    (dimmer-configure-which-key)
    (dimmer-configure-helm)
    (dimmer-mode t)))

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
  :config
  (line-number-mode 1)
  (column-number-mode 1)
  (setq display-time-format "%l:%M%#p") ; e.g. 4:48pm
  (setq display-time-default-load-average 0)
  (display-time-mode 1)
  (setq sml/shorten-directory t)
  (setq sml/shorten-modes t)
  (setq sml/name-width 40)
  (setq sml/mode-width 'full))

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

(setq inhibit-startup-screen t)

(global-hl-line-mode)                   ; highlight current line

(set-frame-font "Inconsolata-12" nil t)

;; Every time a frame is started, make sure it get maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Winner mode allows layout undo [C-c left] & redo [C-c right]
(winner-mode t)

;; Customize the visible bell to flash only the mode line
(setq visible-bell nil)

(defun my-terminal-visible-bell ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))

(setq ring-bell-function #'my-terminal-visible-bell)

;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; TAB navigation
(defun other-window-rev ()
  "other-window with reversed direction for keybinding"
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-<tab>")   'other-window)
(global-set-key (kbd "C-S-<tab>") 'other-window-rev)
