;; MELPA has more up-to-date packages
;; See: https://melpa.org/#/getting-started
;; The MELPA docs say that (require 'package) is not needed
;; but I find that it is needed. Probably add-to-list does
;; not automatically load package like other package related
;; functions would.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
