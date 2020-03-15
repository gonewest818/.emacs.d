;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMACS PACKAGES IN DEVELOPMENT

(use-package quelpa
  :ensure t
  :init (setq quelpa-dir (no-littering-expand-var-file-name "quelpa")))

(use-package quelpa-use-package
  :ensure t
  :after quelpa)

(use-package bufler
  :ensure t
  :demand t
  ;;:quelpa (bufler :fetcher github :repo "alphapapa/bufler.el")
  :quelpa (bufler :fetcher github :repo "gonewest818/bufler.el" :branch "projectile-fix")
  :init
  (bind-key "C-x C-b" #'bufler)
  (bind-key "C-x b" #'bufler-workspace-switch-buffer)
  (setq bufler-groups
        (bufler-defgroups
          (group
           ;; Subgroup collecting all named workspaces.
           (auto-workspace))
          (group
           (group-or "Org Files"
                     (dir (if (bound-and-true-p org-directory)
                              org-directory
                            "~/org"))
                     (mode-match "org-agenda" (rx bos "org-agenda-")))
           (group
            ;; Subgroup collecting indirect Org buffers, grouping them by file.
            ;; This is very useful when used with `org-tree-to-indirect-buffer'.
            (auto-indirect)
            (auto-file))
           ;; Group remaining buffers by whether they're file backed, then by mode.
           (group-not "*special*" (auto-file))
           (auto-mode))
          (group
           (lambda (b)
             (if-let ((s (and (boundp 'erc-server-process)
                              (buffer-local-value 'erc-server-process b))))
                 (format "ERC: %s"
                         (buffer-name (process-buffer s))))))
          (group
           (group-or "RSS Feeds"
                     (name-match "elfeed" (rx bos "*elfeed-"))
                     (name-match "eww" (rx bos "*eww"))))
          (group
           ;; Subgroup collecting buffers in a projectile project.
           (auto-projectile)
           )
          (group
           ;; Subgroup collecting buffers in a version-control project,
           ;; grouping them by directory.
           (auto-project))
          (group
           ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
           (group-or "*Help/Info*"
                     (mode-match "*Help*" (rx bos "help-"))
                     (mode-match "*Info*" (rx bos "info-"))))
          (group
           (group-or "*Docker*"
                     (name-match "state"
                                 (rx bos "*docker-"))
                     (name-match "shells"
                                 (rx bos "* docker run"))))
          (group
           ;; Subgroup collecting all special buffers (i.e. ones that are not
           ;; file-backed), except `magit-status-mode' buffers (which are allowed to fall
           ;; through to other groups, so they end up grouped with their project buffers).
           (group-and "*Special*"
                      (lambda (buffer)
                        (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
                                             buffer)
                                    (funcall (mode-match "Dired" (rx bos "dired"))
                                             buffer)
                                    (funcall (auto-file) buffer))
                          "*Special*")))
           (group
            ;; Subgroup collecting these "special special" buffers
            ;; separately for convenience.
            (name-match "**Special**"
                        (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace") "*")))
           (group
            ;; Subgroup collecting all other Magit buffers, grouped by directory.
            (mode-match "*Magit* (non-status)" (rx bos (or "magit" "forge") "-"))
            (auto-directory))
           ;; Subgroup for Helm buffers.
           (mode-match "*Helm*" (rx bos "helm-"))
           ;; Remaining special buffers are grouped automatically by mode.
           (auto-mode))
          ;; All buffers under "~/.emacs.d" (or wherever it is).
          (dir user-emacs-directory)
          ;; Group remaining buffers by directory, then major mode.
          (auto-directory)
          (auto-mode))))

(use-package deferred
  :ensure t)

(use-package request-deferred
  :ensure t)

(defun tablist--patch-wisent-total-conflicts (fn &rest args)
  "Monkey patch `wisent-total-conflicts' to work around errors in 27.0.90.
Supply a fictitious `load-file-name' to satisfy the code, and
then call FN with ARGS.  This patch can be removed when the bug
is resolved: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=39911"
  (if (wisent-source)
      (apply fn args)
    (let ((load-file-name "tablist-fictitious-file.el"))
      (apply fn args))))

(unless (version< "27" emacs-version)
  (advice-add 'wisent-total-conflicts
              :around #'tablist--patch-wisent-total-conflicts))

(use-package sf511
  :load-path "~/.emacs.d/dev/sf511.el"
  :bind (("C-c qs" . sf511-operators)))

(use-package mincal
  :load-path "~/.emacs.d/dev/mincal.el"
  :commands (mincal-display mincal-retrieve)
  :bind ("C-c qm" . mincal-display))
