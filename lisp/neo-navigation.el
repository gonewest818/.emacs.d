;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NAVIGATION, SEARCH AND COMPLETION

(use-package company
  :ensure t
  :diminish "co"
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 2)
  (bind-keys :map company-active-map
             ("<tab>"   . company-complete-common-or-cycle)
             ("S-<tab>" . company-select-previous)))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-wrap t)
  (ivy-mode t))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
         :map swiper-map
         ("C-c C-t" . swiper-isearch-toggle)
         :map isearch-mode-map
         ("C-c C-t" . swiper-isearch-toggle)))

(use-package counsel
  :ensure t
  :diminish "cn"
  :config
  (counsel-mode t))

(use-package projectile
  :ensure t
  :diminish "prj"
  :bind-keymap (("C-c p" . projectile-command-map))
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-sort-order 'recently-active)
  (setq projectile-project-search-path
        (if neo-config-for-df
            '("~/.emacs.d" "~/work"
              "~/work/mousetrap" "~/work/opensource" "~/work/github")
          '("~/.emacs.d" "~/work")))
  (setq projectile-project-root-functions
        '(projectile-root-local
          projectile-root-marked
          projectile-root-top-down
          projectile-root-top-down-recurring
          projectile-root-bottom-up))
  (setq projectile-switch-project-action #'projectile-dired)
  (projectile-global-mode 1))

(use-package projectile-ripgrep
  :ensure t
  :commands (projectile-ripgrep))

(use-package ibuffer-projectile
  :ensure t
  :commands (ibuffer-projectile-set-filter-groups
             ibuffer-projectile-generate-filter-groups)
  :init
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil)
  ;; Group non-file buffers via their default-directory
  (defvar neo/ibuffer-projectile-nonfile-allowlist-regexps
    '("\\`\\*aidermacs:.*\\*\\'"          ; *aidermacs:{path}*
      "\\`OpenCode Agent @ \\S-+\\'"      ; OpenCode Agent @ {path}
      )
    "Regexps of non-file buffer names allowed to be grouped by projectile.")
  (defun neo/ibuffer-projectile--allowed-nonfile-buffer-p (buf)
    (let ((name (buffer-name buf)))
      (seq-some (lambda (re) (string-match-p re name))
                neo/ibuffer-projectile-nonfile-allowlist-regexps)))
  (defun neo/ibuffer-projectile-root--fallback-with-allowlist (orig buf)
    "Let ibuffer-projectile group allowlisted non-file buffers via `default-directory`."
    (or (funcall orig buf)
        (when (neo/ibuffer-projectile--allowed-nonfile-buffer-p buf)
          (with-current-buffer buf
            (when (and default-directory (file-directory-p default-directory))
              (let ((root (ignore-errors
                            (let ((default-directory default-directory))
                              (projectile-project-root)))))
                (when root
                  (cons (let ((default-directory root))
                          (projectile-project-name))
                        root))))))))
  (advice-add 'ibuffer-projectile-root :around
              #'neo/ibuffer-projectile-root--fallback-with-allowlist)
  ;; Works similarly to the ibuffer-projectile default, but we have a
  ;; few more custom filter groups we want to concatenate to the list.
  (defun neo/ibuffer-projectile--set-filter-groups ()
    (interactive)
    (setq ibuffer-filter-groups
          (nconc (ibuffer-projectile-generate-filter-groups)
                 '(("irc"    (mode . erc-mode))
                   ("ement"  (name . "^\\*Ement Room"))
                   ("browse" (or (mode . elfeed-search-mode)
                                 (name . "^\\*elfeed-.*\\*$")
                                 (mode . eww-mode)))
                   ("slack"  (mode . slack-message-buffer-mode))
                   ("sf511"  (name . "^\\*sf511-.*\\*$")))))
    (message "ibuffer: filter groups set")
    (let ((ibuf (get-buffer "*Ibuffer*")))
      (when ibuf
        (with-current-buffer ibuf
          (pop-to-buffer ibuf)
          (ibuffer-update nil t)))))
  (add-hook 'ibuffer-hook #'neo/ibuffer-projectile--set-filter-groups))

(use-package avy
  :ensure t
  :bind (("M-g g" . avy-goto-line)
         ("C-:"   . avy-goto-char)
         ("C-\""  . avy-goto-char-2)))

(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-prefer-searcher 'rg))

(use-package vlf
  :ensure t
  :config (require 'vlf-setup))

;; when visiting emacs source buffers, make them read-only
(dir-locals-set-class-variables
 'emacs-sources
 '((nil . ((buffer-read-only . t)))))
(mapc (lambda (d)
        (dir-locals-set-directory-class d 'emacs-sources))
      (list "/usr/local/share/emacs"
            "/Applications/Emacs.app/Contents/Resources"))
