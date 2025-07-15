;;; lisp/my-kbd-macros.el --- Keyboard macros -*- lexical-binding: t -*-

(defalias 'kmacro-insert-macro 'insert-kbd-macro)

(defalias 'targets_string_to_tar_read
   (kmacro "w w v w w w w w h y 0 p a SPC < - SPC t a r _ r e a d ( <kp-delete> <kp-delete> <kp-delete> <escape> $ a <backspace> ) <escape>"))


(provide 'my-kbd-macros)
;;; my-kbd-macros.el ends here
