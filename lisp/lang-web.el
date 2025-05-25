;;; lang-web.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2023  John Sperger

;; Author: John Sperger

;;; Commentary:

;;; Code:

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
	;; org-mode and http requests

	;; structure specifications for HTTP requests using Org's tree structure.
	;; Properties defined in the child headings extend or override properties
	;; defined in the parent headings - this way, it is possible to author HTTP
	;; requests without having to repeat things such as URL hosts, headers, ports,
	;; etc.
	)


(provide 'lang-web)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; lang-web.el ends here
