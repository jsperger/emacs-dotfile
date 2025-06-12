emacs-lisp-mode elisp-mode

(comment-line-40 ";; -------------------------------------" n)

(header ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name)))
        " --- " p " -*- lexical-binding: t -*-" n
        ";;; Commentary:" n ";;; Code:" n n)


(provides "(provide '"
					(file-name-base (or (buffer-file-name) (buffer-name))) ")" n
          ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name)))
          " ends here" n)

(config-template ";;; "
								 (file-name-nondirectory (or (buffer-file-name) (buffer-name)))
                 " --- " p " -*- lexical-binding: t -*-" n
                 ";;; Commentary:" n
                 ";;; Code:" n n n
                 "(provide '"
								 (file-name-base (or (buffer-file-name) (buffer-name))) ")" n
                 ";; Local Variables:" n
                 ";; no-byte-compile: t" n
                 ";; no-native-compile: t" n
                 ";; no-update-autoloads: t" n
                 ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name)))
                 " ends here" n)

(footer-vars ";;; Local Variables:
;;; no-byte-compile: t
;;; no-native-compile: t
;;; no-update-autoloads: t
;;; End:")

(footer-emacs-config "
(provide '" (file-name-base)  ")
;;; Local Variables:
;;; no-byte-compile: t
;;; no-native-compile: t
;;; no-update-autoloads: t
;;; End:" n
";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name))) " ends here")
