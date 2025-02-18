;;; lang-python.el --- summary -*- lexical-binding: t -*-

;; Author: John Sperger


;;; Commentary:

;; commentary

;;; Code:

(use-package python-ts-mode
	:ensure nil
	:mode ("\\.py\\'" . python-ts-mode)
;;	:hook (python-ts-mode . eglot-ensure)
	;; ^caused an infinite loop

	:config
	(add-to-list 'eglot-server-programs
             '((python-mode python-ts-mode)
               "basedpyright-langserver" "--stdio"))
	)

(provide 'lang-python)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; lang-python.el ends here
