;;; my-llm-helpers.el --- Helper functions for LLM tools -*- lexical-binding: t -*-

(declare-function agent-shell-cwd "agent-shell-project")
(declare-function no-littering-expand-etc-file-name "no-littering")

(defun my/agent-shell-dot-subdir (subdir)
  "Return SUBDIR of a per-project agent-shell data directory under etc/.
The project path is flattened into the directory name, e.g.
etc/agent-shell/Users-me-src-proj/SUBDIR, so nothing is written into
the project itself."
  (let* ((cwd (directory-file-name (agent-shell-cwd)))
         (project (replace-regexp-in-string
                   "/" "-" (string-remove-prefix "/" cwd))))
    (expand-file-name
     subdir
     (no-littering-expand-etc-file-name (concat "agent-shell/" project)))))

(provide 'my-llm-helpers)
;;; my-llm-helpers.el ends here
