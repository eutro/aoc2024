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

(defvar-local aoc-pinned-day-number nil)

(defun aoc-day-number ()
  "Determine the day number of the current file, or the current day."
  (or
   aoc-pinned-day-number
   (when-let ((file-name (buffer-file-name)))
     (and (string-match aoc-dayfile-pattern file-name)
          (string-to-number (match-string 1 file-name))))
   (cadr (calendar-current-date))))

(defun aoc-get-out-buffer (&optional clear day)
  "Get the *aoc-output* buffer."
  (let ((buf (get-buffer-create "*aoc-output*")))
    (when (or clear
              (not (eq 'aoc-run-mode
                       (buffer-local-value 'major-mode buf))))
      (with-current-buffer buf
        (erase-buffer)
        (when day (setq aoc-pinned-day-number day))
        (aoc-run-mode)))
    buf))

(defun aoc-run (&optional prefix)
  "Run the current day.

With PREFIX, read input from the buffer."
  (interactive "P")
  (let* ((day (aoc-day-number))
         (inp-args (when prefix '("--")))
         (buf (aoc-get-out-buffer t day))
         (args (cons (number-to-string day) inp-args))
         (proc (apply
                #'start-process
                "aoc-run" buf
                (expand-file-name "run.sh" aoc-root)
                args))
         (win (selected-window)))
    (unless (eq (window-buffer win) buf)
      (pop-to-buffer buf 'display-buffer-use-least-recent-window))
    (message "./run.sh %s" (string-join (mapcar #'shell-quote-argument args) " "))
    (unless prefix (select-window win))))

(defun aoc-copy-part-answer (part)
  "Copy the answer for the given PART from *aoc-output*."
  (with-current-buffer (aoc-get-out-buffer)
    (goto-char (point-max))
    (unless (re-search-backward (format "Part %d: " part) nil t)
      (user-error "Part %d not found in buffer" part))
    (goto-char (match-end 0))
    (let ((start (point)))
      (end-of-line)
      (copy-region-as-kill start (point))
      (message "Copied: %s" (current-kill 0 t)))))

(defmacro aoc-copy-n (part)
  `(defun ,(intern (format "aoc-copy-%d" part)) ()
     ,(format "Copy the answer for part %d in *aoc-output*." part)
     (interactive)
     (aoc-copy-part-answer ,part)))

(aoc-copy-n 1)
(aoc-copy-n 2)

(defconst aoc-mode-map
  (let ((keys (make-sparse-keymap)))
    (define-key keys (kbd "C-c C-c") #'aoc-run)
    (define-key keys (kbd "C-c 1") #'aoc-copy-1)
    (define-key keys (kbd "C-c 2") #'aoc-copy-2)
    keys))

(define-minor-mode aoc-mode
  "Minor mode for Advent of Code utilities"
  :lighter " AoC24")

(defconst aoc-run-mode-map
  (let ((keys (copy-keymap aoc-mode-map)))
    keys))

(define-derived-mode aoc-run-mode comint-mode "AoC-Run"
  "Major mode for the *aoc-run* buffer.")

;; Local Variables:
;; read-symbol-shorthands: (("aoc-" . "aoc2024-eutro-"))
;; End:

(provide 'aoc2024-eutro)
;;; aoc2024-eutro.el ends here
