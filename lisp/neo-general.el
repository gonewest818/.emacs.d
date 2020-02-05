;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL SETTINGS

(let ((bd-path (concat user-emacs-directory "dev/better-defaults")))
  ;; we have a locally patched version that doesn't override no-littering
  (message "using dev version of better-defaults")
  (if (file-directory-p bd-path)
      (progn
        (add-to-list 'load-path bd-path)
        (use-package better-defaults))
    ;; whereas in the published version we need to reset some paths
    (use-package better-defaults
      :ensure t
      :config
      (message "using melpa version of better-defaults")
      (setq save-place-file                 ; override the better-default
            (no-littering-expand-var-file-name "places"))
      (setq backup-directory-alist          ; override the better-default
            `(("." . ,(no-littering-expand-var-file-name "backups")))))))

(set-language-environment "UTF-8")

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
(setq lpr-page-header-switches '("-l" "80")) ; lines per page

;; Dired configuration
;; See https://www.emacswiki.org/emacs/DiredReuseDirectoryBuffer
(put 'dired-find-alternate-file 'disabled nil)
