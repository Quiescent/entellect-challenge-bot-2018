;;; display-words --- Display functions for debugging

;;; Comentary

;;; Code

(require 'calc-bin)
(require 'dash)

(defun quiescent-display-word64 (x)
  "Display the word64 X left-padded with zeros up to 64 digits."
  (interactive "nNumber: ")
  (let* ((calc-number-radix 2)
         (binary            (math-format-radix x))
         (zeros             (- 64 (length binary))))
    (message (concat (-repeat zeros ?0) binary))))

(provide 'display-words)
;;; display-words.el ends here
