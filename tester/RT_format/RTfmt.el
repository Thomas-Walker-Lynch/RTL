(defun RTfmt-buffer ()
  "Format the current buffer using RTfmt."
  (interactive)
  (if (not (executable-find "RTfmt"))
      (message "Error: RTfmt executable not found in PATH.")
    (let ((temp-buffer (generate-new-buffer " *RTfmt*"))
          (args (list "pipe")))
      (when (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
        (setq args (append args (list "--lisp"))))
      (unwind-protect
          (let ((exit-code (apply #'call-process-region
                                  (point-min) (point-max)
                                  "RTfmt"
                                  nil temp-buffer nil
                                  args)))
            (if (zerop exit-code)
                (progn
                  ;; Applies a non-destructive diff, preserving point and markers natively
                  (replace-buffer-contents temp-buffer)
                  (message "RTfmt formatting successful."))
              (message "RTfmt failed with exit code %s. Buffer unchanged." exit-code)))
        (kill-buffer temp-buffer)))))
