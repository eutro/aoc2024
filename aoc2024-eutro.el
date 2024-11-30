;;; aoc2024-eutro.el --- Advent of Code utilities -*- lexical-binding: t -*-

;;; Code:

(require 'smalltalk-mode)

(defgroup aoc2024-eutro nil
  "Group for Eutro's Advent of Code 2024"
  :prefix 'aoc2024-eutro-
  :group 'misc)

(defconst aoc-root
  (locate-dominating-file
   (or load-file-name (buffer-file-name))
   "Tupfile.ini"))

(defconst aoc-dayfile-pattern "day\\([0-9]\\{2\\}\\)")

(defun aoc-day-number ()
  "Determine the day number of the current file, or the current day."
  (or
   (let ((file-name (buffer-file-name)))
     (and (string-match aoc-dayfile-pattern file-name)
          (string-to-number (match-string 1 file-name))))
   (cadr (calendar-current-date))))

(defun aoc-run ()
  "Run the current day."
  (interactive)
  (let ((default-directory aoc-root))
    (async-shell-command
     (format  "./run.sh %s" (aoc-day-number))
     "*aoc-output*"
     "*aoc-error*")))

(defconst aoc-mode-map
  (let ((keys (make-sparse-keymap)))
    (define-key keys (kbd "C-c C-c") #'aoc-run)
    keys))

(define-minor-mode aoc-mode
  "Minor mode for Advent of Code utilities"
  :lighter " AoC24")

;; Local Variables:
;; read-symbol-shorthands: (("aoc-" . "aoc2024-eutro-"))
;; End:

(provide 'aoc2024-eutro)
;;; aoc2024-eutro.el ends here
