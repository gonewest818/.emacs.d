;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EARLY INITIALIZATION

;; Package system setup (before GUI loads)
(setq package-enable-at-startup nil)

;; Performance optimizations
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; UI optimizations (prevent flash)
(setq frame-inhibit-implied-resize t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Configure native compilation cache directory
(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory)))
  ;; Put primitive-call trampolines under var/ too; otherwise Emacs may
  ;; recreate ~/.emacs.d/eln-cache for these generated .eln files.
  (setq native-comp-enable-subr-trampolines
        (expand-file-name "var/eln-cache/trampolines/" user-emacs-directory)))
