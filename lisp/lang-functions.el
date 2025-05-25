;;; lang-functions.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;; Functions for specific programming languages
;;; Code:



; ================================== R ================================== ;
(defun oc-set-ess-offset-indent ()
	"Set ess-offset-arguments values to my preferred defaults. Motivated by an Org src block issue (that  is likely due to a config issue on my end)."
	(interactive)
	(setopt
	 ess-offset-arguments 'prev-call
	 ess-offset-arguments-newline 'prev-line)
	)

(defun air-format ()
	"Run the 'air' formatter in the current directory. Runs asynchronously"
	(interactive)
	(async-shell-command "air format .")
	)


(provide 'lang-functions)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; lang-functions.el ends here
