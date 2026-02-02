;; -*- lexical-binding: t -*-
;; When using magit don't let vc get in the way
(setq vc-handled-backends (delq 'Git vc-handled-backends))
