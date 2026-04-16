;;; native-comp-exclusions.el --- Shared native compilation exclusions -*- lexical-binding: t; -*-

(defconst neo-native-comp-exclusion-regexps
  '("/dired-k-[^/]+/" ; "No such file or directory: direx-project"
    "/magit-[^/]+/" ; see github.com:magit/magit/issues/5557
    )
  "Package path regexps to exclude from native compilation.

Each exclusion should correspond to a known upstream issue.")
