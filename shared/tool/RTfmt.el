(defun RTfmt0-buffer ()
  "Format the current buffer using RTfmt0."
  (interactive)
  (if (not (executable-find "RTfmt0"))
      (message "Error: RTfmt0 executable not found in PATH.")
    (let ((temp-buffer (generate-new-buffer " *RTfmt0*"))
          (args (list "pipe")))
      (when (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
        (setq args (append args (list "--lisp"))))
      (unwind-protect
          (let ((exit-code (apply #'call-process-region
                                  (point-min) (point-max)
                                  "RTfmt0"
                                  nil temp-buffer nil
                                  args)))
            (if (zerop exit-code)
                (progn
                  ;; Applies a non-destructive diff, preserving point and markers natively
                  (replace-buffer-contents temp-buffer)
                  (message "RTfmt0 formatting successful."))
              (message "RTfmt0 failed with exit code %s. Buffer unchanged." exit-code)))
        (kill-buffer temp-buffer)))))
