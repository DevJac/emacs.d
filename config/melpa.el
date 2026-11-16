;; -*- lexical-binding: t; -*-

;; MELPA has more up-to-date packages
;; See: https://melpa.org/#/getting-started

;; The MELPA docs say that (require 'package) is not needed but I find
;; that it is needed. This is probably because add-to-list does not
;; automatically load package like other package related functions
;; would, and so if we want to have package-archive defined so we can
;; add to it, then we need to (require 'package) first.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
