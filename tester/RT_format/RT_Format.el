( defun RT-format-buffer()
  (interactive)
  (save-excursion
    ( shell-command-on-region(point-min)(point-max)
                             "RT_format pipe" t t)) )
