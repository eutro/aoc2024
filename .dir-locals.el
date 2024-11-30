((smalltalk-mode
  (eval . (unless (featurep 'aoc2024-eutro)
            (let ((load-path (cons (locate-dominating-file
                                    (or load-file-name (buffer-file-name))
                                    "aoc2024-eutro.el")
                                   load-path)))
              (require 'aoc2024-eutro))))
  (eval . (aoc2024-eutro-mode))))
