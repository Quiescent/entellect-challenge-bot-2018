;;; highlight-console-replays --- highlight towers in the match replays -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun quiescent-highlight-towers ()
  "Highlight towers in replays."
  (interactive)
  (progn
    (hi-lock-set-pattern "\\[[0-9>< ]*e[0-9><]*\\]" 'hi-blue-b)
    (hi-lock-set-pattern "\\[[0-9>< ]*E[0-9><]*\\]" 'hi-blue-b)
    (hi-lock-set-pattern "\\[[0-9>< ]*a[0-9><]*\\]" 'hi-red-b)
    (hi-lock-set-pattern "\\[[0-9>< ]*A[0-9><]*\\]" 'hi-red-b)
    (hi-lock-set-pattern "\\[[0-9>< ]*d[0-9><]*\\]" 'hi-green-b)
    (hi-lock-set-pattern "\\[[0-9>< ]*D[0-9><]*\\]" 'hi-green-b)
    (hi-lock-set-pattern "\\[[0-9>< ]*t[0-9><]*\\]" 'hi-pink)
    (hi-lock-set-pattern "\\[[0-9>< ]*T[0-9><]*\\]" 'hi-pink)
    (hi-lock-set-pattern "\\[[0-9]*[>< ]+[0-9]*\\]" 'hi-black-b)))

(provide 'highlight-console-replays)
;;; highlight-console-replays ends here
