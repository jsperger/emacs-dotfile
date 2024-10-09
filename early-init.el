;;;; early-init.el --- -*- lexical-binding: t; -*-

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (defun reset-gc-cons-threshold ()
            (setq gc-cons-threshold 100000000 gc-cons-percentage 0.1)))

;; Inhibit package initialize
(setq package-enable-at-startup nil)

;; Set elpaca-menu-functions in priority order (I think)
;; i.e. devel preferred over standard, we're living on the edge
(setq elpaca-menu-functions '(elpaca-menu-extensions
                              elpaca-menu-non-gnu-devel-elpa
                              elpaca-menu-gnu-devel-elpa
                              elpaca-menu-org
                              elpaca-menu-melpa
                              elpaca-menu-non-gnu-elpa
                              elpaca-menu-gnu-elpa))

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Inhibit byte-compiler warnings
(setq byte-compile-warnings nil)

;; Remove some unneeded UI elements
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(internal-border-width . 0) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))


;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
