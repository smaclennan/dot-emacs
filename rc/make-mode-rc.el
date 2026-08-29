;; -*- lexical-binding: t; no-byte-compile: t -*-
;; So makefiles get nice compile commands
(require 'my-compile)
(add-hook 'makefile-mode-hook 'my-compile-command t)
