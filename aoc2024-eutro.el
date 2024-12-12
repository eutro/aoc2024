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
   (progn
     (require 'calendar)
     (cadr (calendar-current-date)))))

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

(defconst aoc--run-script (expand-file-name "run.sh" aoc-root))
(defconst aoc--explore-script (expand-file-name "explore.sh" aoc-root))

(defconst aoc--display-buffer-action nil
  ;;'display-buffer-use-least-recent-window
  )

(defun aoc-run (&optional prefix)
  "Run the current day.

With PREFIX, read input from the buffer."
  (interactive "P")
  (let* ((day (aoc-day-number))
         (inp-args (when prefix '("--")))
         (buf (aoc-get-out-buffer t day))
         (args (cons (number-to-string day) inp-args))
         (proc (apply #'start-process "aoc-run" buf aoc--run-script args))
         (win (selected-window)))
    (set-process-sentinel
     proc
     (lambda (_proc msg)
       (when-let ((win (get-buffer-window buf)))
         (with-selected-window win 
           (recenter -1)))))
    (unless (eq (window-buffer win) buf)
      (if prefix
          (pop-to-buffer buf aoc--display-buffer-action)
        (display-buffer buf aoc--display-buffer-action)))
    (message "./run.sh %s" (string-join (mapcar #'shell-quote-argument args) " "))))

(defun aoc-explore--filter (proc output)
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (point-max))
      (insert output))))

(defun aoc-explore--sentinel (proc status)
  ;(message status)
  nil)

(defun aoc-explorer ()
  "Start an interactive explorer for the current day."
  (interactive)
  (let* ((day (aoc-day-number))
         (args (list aoc--explore-script (number-to-string day)))
         (buf (get-buffer-create "*aoc-explore*"))
         (proc (get-buffer-process buf)))
    (when (process-live-p proc)
      (process-send-eof proc))
    (with-current-buffer buf
      (erase-buffer)
      (setq aoc-pinned-day-number day)
      (org-mode)
      (visual-line-mode))
    (setq proc
          (make-process
           :name "aoc-explore"
           :buffer buf
           :command args
           :stderr (get-buffer-create "*aoc-explore error*")
           :filter #'aoc-explore--filter
           :sentinel #'aoc-explore--sentinel))
    (pop-to-buffer buf aoc--display-buffer-action)
    proc))

(defun aoc-explorer--find ()
  "Find or create the explorer process."
  (if current-prefix-arg
      (aoc-explorer)
    (let (buf proc)
      (setq buf (get-buffer "*aoc-explore*"))
      (when buf (setq proc (get-buffer-process buf)))
      (if (process-live-p proc)
          (progn
            (pop-to-buffer buf aoc--display-buffer-action)
            proc)
        (aoc-explorer)))))

(defun aoc-explore-command (cmd)
  "Clear the buffer and send the AoC explorer the specific command."
  (interactive "MCommand: ")
  (let ((proc (aoc-explorer--find))
        (cmd (string-join (list (string-replace "\n" "" cmd) "\n"))))
    (with-current-buffer (process-buffer proc)
      (erase-buffer)
      (process-send-string proc cmd))))

(defun aoc-explore (what)
  (interactive "MExplore: ")
  (aoc-explore-command (format "explore %s" what)))

(defun aoc-follow-link (path prefix)
  "Follow the AoC link PATH with universal PREFIX argument."
  (message "Following: %s" path)
  (aoc-explore-command (format "resolve %s" path)))

(defun aoc-list-classes ()
  "List all the classes."
  (interactive)
  (aoc-explore-command "list classes"))

(defun aoc-list-selectors ()
  "List all the selectors."
  (interactive)
  (aoc-explore-command "list selectors"))

(with-eval-after-load 'ol
  (setf (alist-get "aoc2024eutro" org-link-parameters nil nil #'equal)
        (list :follow #'aoc-follow-link)))

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
