;;; lang/zsh/config.el -*- lexical-binding: t; -*-

(after! sh-script
  (when (modulep! :editor format)
    (set-formatter! 'beautysh
      '("beautysh" "-"
        ("-i" "%d" (unless indent-tabs-mode tab-width)))
      :modes '((sh-mode (string= sh-shell "zsh"))))))
