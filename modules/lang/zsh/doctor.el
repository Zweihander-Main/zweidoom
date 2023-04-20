;;; lang/zsh/doctor.el -*- lexical-binding: t; -*-

(when (modulep! :editor format)
  (unless (executable-find "beautysh")
    (warn! "Couldn't find beautysh. Code formatting will not work.")))
