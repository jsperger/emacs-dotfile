;;; early-init.el --- before package.el, GUI -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; [[file:its-lit.org::#optimizations-for-early-init][Optimizations for early-init:1]]
;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (defun reset-gc-cons-threshold ()
            (setq gc-cons-threshold 100000000 gc-cons-percentage 0.1))
          )
;; Optimizations for early-init:1 ends here

;; [[file:its-lit.org::#disable-package-initialization-for-elpaca][Disable package initialization for elpaca:1]]
;; Inhibit package initialize
(setq package-enable-at-startup nil)
;; Disable package initialization for elpaca:1 ends here

;; [[file:its-lit.org::#configure-gui-elements][Configure GUI elements:1]]
;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Remove some unneeded UI elements
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(internal-border-width . 0) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist)
  (setq frame-resize-pixelwise t
        window-resize-pixelwise t
        ns-pop-up-frames nil)
  )
;; Configure GUI elements:1 ends here

;; [[file:its-lit.org::#disable-byte-compilation-warnings][Disable byte compilation warnings:1]]
;; Inhibit byte-compiler warnings
(setq byte-compile-warnings nil)
;; Disable byte compilation warnings:1 ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; early-init.el ends here
