;;; native-compile-packages.el --- Native compile installed packages -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'comp)

(load (expand-file-name "etc/native-comp-exclusions.el" user-emacs-directory) nil t)

(let ((files (sort (seq-remove
                    (lambda (file)
                      (or (string-match-p "/\\.dir-locals\\.el\\'" file)
                          (string-match-p "-autoloads\\.el\\'" file)
                          (string-match-p "-pkg\\.el\\'" file)
                          (seq-some (lambda (regexp)
                                      (string-match-p regexp file))
                                    (if (boundp 'neo-native-comp-exclusion-regexps)
                                        neo-native-comp-exclusion-regexps
                                      nil))))
                    (directory-files-recursively package-user-dir "\\.el\\'"))
                   #'string<))
      (skipped 0)
      (compiled 0)
      failures)
  (dolist (file files)
    (let ((eln-file (comp-lookup-eln file)))
      (if (and eln-file
               (file-exists-p eln-file)
               (not (file-newer-than-file-p file eln-file)))
          (setq skipped (1+ skipped))
        (message "Native compiling %s" file)
        (condition-case err
            (progn
              (native-compile file)
              (setq compiled (1+ compiled)))
          (error
           (push (cons file err) failures)
           (message "Native compile failed for %s: %S" file err))))))
  (if failures
      (progn
        (message "Native compilation finished: %d compiled, %d skipped, %d failure(s)"
                 compiled skipped (length failures))
        (dolist (failure (nreverse failures))
          (message "  %s: %S" (car failure) (cdr failure)))
        (kill-emacs 1))
    (message "Native compilation finished: %d compiled, %d skipped"
             compiled skipped)))
