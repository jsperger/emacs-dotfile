;;; config/setup-web.el --- Web development configuration -*- lexical-binding: t -*-

(use-package ox-hugo
	:disabled
	)

(use-package easy-hugo
	:disabled
  :init
  (setq
   easy-hugo-basedir "~/code/website/jsperger-hugo/"
	 easy-hugo-url "https://jsperger.github.io"
	 )
	:config
	(setopt
	 easy-hugo-no-help t
	 )
	(easy-hugo-enable-menu)
	:general
	(tyrant-def "ah" 'easy-hugo-menu)
	)

(use-package htmlize)

(use-package verb
	:disabled
	)

;;; setup-web.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
