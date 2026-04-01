;;;--------------------------------------------------------------------------------
;;; Emacs support for the RT Literal, RTL
;;;

(require 'color)

;;;------------------------------------------------------------------------------
;;; Utilities
;;;

  (defun RT-literal·string-byte_count (text)
    (if 
      (characterp text)
      (string-bytes (char-to-string text))
      (string-bytes text)
      ))

  (defun RT-literal·list-index (item target_list)
    (let 
      (
        (index 0)
        (found_bool nil)
        )
      (while (and target_list (not found_bool))
        (if 
          (eq (car target_list) item)
          (setq found_bool t)
          (progn
            (setq target_list (cdr target_list))
            (setq index (+ index 1))
            )))
      (if 
        found_bool
        index
        nil
        )))

  ;; Generic Stack Interface
  ;;

  (defmacro RT-literal·stack-push (stack_sym item)
    `(push ,item ,stack_sym)
    )

  (defmacro RT-literal·stack-pop (stack_sym)
    `(pop ,stack_sym)
    )

  (defun RT-literal·stack-top (stack_list)
    (car stack_list)
    )

  (defmacro RT-literal·stack-is-empty (stack_sym)
    `(null ,stack_sym)
    )

  (defun RT-literal·overlay-stack-is-empty ()
    (RT-literal·stack-is-empty RT-literal·overlay-stack)
    )

  ;;  Enables keeping the topological terminology consistent in the code body
  ;;

  (defmacro RT-literal·overlay_leftmost (overlay)
    `(overlay-start ,overlay)
    )

  (defmacro RT-literal·overlay_rightmost_right-neighbor (overlay)
    `(overlay-end ,overlay)
    )

;;;------------------------------------------------------------------------------
;;; Configuration
;;;

  ;; emphasize a character in a cell interpretation
  (setq-local cursor-type 'box)
  (setq-local cursor-in-non-selected-windows 'box)

  (defvar-local left-delimiter "“")
  (defvar-local right-delimiter "”")

  (defvar-local quote-pair_size 
    (RT-literal·string-byte_count (concat left-delimiter right-delimiter))
    )
  (defvar-local min-with-extent_size 
    (RT-literal·string-byte_count (concat left-delimiter "0 " right-delimiter))
    )

  (defvar-local RT-literal·highlight_func 'RT-literal·highlight-shades)
  (defvar-local RT-literal·hide-extent nil)

  ;; Buffer-Local State
  (defvar-local RT-literal·overlay-stack nil)
  (defvar-local RT-literal·selected-overlay nil)

  (defvar RT-literal·target-mode-hooks_list
    (list
      'c-mode-hook
      'c++-mode-hook
      'c-ts-mode-hook
      'c++-ts-mode-hook
      'js-mode-hook
      'typescript-mode-hook
      'python-mode-hook
      'java-mode-hook
      'rust-mode-hook
      'sh-mode-hook
      'html-mode-hook
      'nxml-mode-hook
      'json-mode-hook
      ))

  ;; wrap with indentation
  (defvar-local RT-literal·display-wrap-offset 2)
  (defvar-local RT-literal·display-wrap-enabled t)

;;;------------------------------------------------------------------------------
;;; Implementation
;;;

  ;; Color logic
  ;;

  (defun RT-literal·highlight-none ()
    nil
    )

  (defun RT-literal·highlight-default ()
    'region
    )

  (defun RT-literal·highlight-shades ()
    (let* (
        (bg-raw_str (face-background 'default nil t))
        (fg-raw_str (face-foreground 'default nil t))
        (bg-color_str (if 
                        (or (null bg-raw_str) (string= bg-raw_str "unspecified-bg")) 
                        "#000000" 
                        bg-raw_str))
        (fg-color_str (if 
                        (or (null fg-raw_str) (string= fg-raw_str "unspecified-fg")) 
                        "#FFFFFF" 
                        fg-raw_str))
        (bg-hsl_list (apply 'color-rgb-to-hsl (color-name-to-rgb bg-color_str)))
        (fg-hsl_list (apply 'color-rgb-to-hsl (color-name-to-rgb fg-color_str)))
        (bg_h (nth 0 bg-hsl_list))
        (bg_s (nth 1 bg-hsl_list))
        (bg_l (nth 2 bg-hsl_list))
        (fg_h (nth 0 fg-hsl_list))
        (fg_s (nth 1 fg-hsl_list))
        (fg_l (nth 2 fg-hsl_list))
        (is-dark_bool (eq (frame-parameter nil 'background-mode) 'dark))
        (new-bg_l 0.0)
        (new-fg_l 0.0)
        )
      (if 
        is-dark_bool
        (progn
          (setq new-bg_l (- bg_l 0.15))
          (if 
            (< new-bg_l 0.0) 
            (setq new-bg_l (+ bg_l 0.15))
            )
          (setq new-fg_l (+ fg_l 0.10))
          (if 
            (> new-fg_l 1.0) 
            (setq new-fg_l 1.0)
            )
          (setq bg_s (min 1.0 (+ bg_s 0.1))) 
          )
        (progn
          (setq new-bg_l (+ bg_l 0.15))
          (if 
            (> new-bg_l 1.0) 
            (setq new-bg_l (- bg_l 0.15))
            )
          (setq new-fg_l (- fg_l 0.10))
          (if 
            (< new-fg_l 0.0) 
            (setq new-fg_l 0.0)
            )
          (setq bg_s (min 1.0 (+ bg_s 0.1)))
          ))
      (list
        :background (apply 'color-rgb-to-hex (color-hsl-to-rgb bg_h bg_s new-bg_l))
        :foreground (apply 'color-rgb-to-hex (color-hsl-to-rgb fg_h fg_s new-fg_l))
        )))

  (defun RT-literal·apply-highlight (overlay)
    (let 
      ( 
        (face_prop (funcall RT-literal·highlight_func)) 
        )
      (if 
        face_prop
        (overlay-put overlay 'face face_prop)
        (overlay-put overlay 'face nil)
        )))

  (defun RT-literal·apply-selection (overlay)
    (let* (
        (base-face_list (funcall RT-literal·highlight_func))
        (fg-color_str (plist-get base-face_list :foreground))
        (selected-face_list (append base-face_list (list :box (list :line-width 1 :color fg-color_str))))
        )
      (if 
        base-face_list
        (overlay-put overlay 'face selected-face_list)
        )))

  ;; string manipulation
  ;;

  (defun RT-literal·strip-outer-quotes (text)
    (let 
      (
        (len (length text))
        )
      (if 
        (>= len 2)
        (let 
          (
            (first-char (substring text 0 1))
            (last-char (substring text (- len 1) len))
            )
          (if 
            (and (string= first-char last-char) (or (string= first-char "\"") (string= first-char "'")))
            (substring text 1 (- len 1))
            text
            ))
        text
        )))

    (defun RT-literal·unescape-string (text)
      (replace-regexp-in-string
        "\\\\\\(.\\)"
        (lambda (match_str)
          (let 
            (
              (char_str (substring match_str 1 2))
              )
            (pcase char_str
              ("n" "\n")
              ("t" "\t")
              ("r" "\r")
              (_ char_str)
              )))
        text t t)
      )

  ;; Form logic
  ;;

  ;; By contract: when this is called overlay is known to be large enough to hold both delimiters
  (defun RT-literal·is-quoted-form (overlay_leftmost overlay_rightmost_right-neighbor)
    (let
      (
        (left_len (length left-delimiter))
        (right_len (length right-delimiter))
        )
      (and
        (string=
          (buffer-substring-no-properties overlay_leftmost (+ overlay_leftmost left_len))
          left-delimiter
          )
        (string=
          (buffer-substring-no-properties (- overlay_rightmost_right-neighbor right_len) overlay_rightmost_right-neighbor)
          right-delimiter
          ))))

  ;; By contract: only called when overlay holds a newly made, non-null, RTL.
  ;; Everything from content_leftmost to content_rightmost is data, inclusive.
  (setq RT-literal·describe-new-form_status (list 'new-good))
  (defun RT-literal·describe-new-form_has-error (status)
    nil
    )
  (defun RT-literal·describe-new-form_message (status)
    nil
    )
  (defun RT-literal·describe-new-form (overlay_leftmost content_rightmost_right-neighbor)
    (let*
      (
        (content_leftmost (+ overlay_leftmost (length left-delimiter)))
        (content-size (- 
                (position-bytes content_rightmost_right-neighbor) 
                (position-bytes content_leftmost)))
        )
      (list 'new-good content_leftmost content-size)
      ))

  ;; By contract, this is only called when the overlay is over a well formed extent literal.
  ;; By contract: only called when the overlay holds well formed non-null extent literal.
  ;; For example
  ;;   “6 golfing” - extent, space, then data
  (setq  
    RT-literal·describe-existing-form_status 
    (list 
      'edited-missing-extent-field 
      'edited-content-negative-extent
      'edited-null-content
      'edited-good
      ))
  (defun RT-literal·describe-existing-form_has-error (status)
    (if 
      (memq status '(edited-missing-extent-field edited-content-negative-extent))
      t
      nil
      ))
  (defun RT-literal·describe-existing-form_message (status)
    (pcase status
      ('edited-missing-extent-field
        (message "RT-literal·describe-existing-form:: missing extent string")
        )
      ('edited-content-negative-extent
        (message "RT-literal·describe-existing-form:: malformed existing literal")
        )
      ))
  (defun RT-literal·describe-existing-form (overlay_leftmost content_rightmost_right-neighbor)
    (let
      (
        (extent_field_leftmost (+ overlay_leftmost (length left-delimiter)))
        )
      (save-match-data
        (save-excursion
          (goto-char extent_field_leftmost)
          (if 
            (not (looking-at "[0-9a-fA-F]+ "))
            (list 'edited-missing-extent-field)
            (let 
              (
                (content_leftmost (match-end 0))
                )
              (if 
                (> content_leftmost content_rightmost_right-neighbor)
                (list 'edited-content-negative-extent)
                (if 
                  (= content_leftmost content_rightmost_right-neighbor)
                  (list 'edited-null-content extent_field_leftmost content_leftmost)
                  (let 
                    (
                      (content-size (- 
                                (position-bytes content_rightmost_right-neighbor) 
                                (position-bytes content_leftmost)))
                      )
                    (list 'edited-good extent_field_leftmost content_leftmost content-size))
                  ))))))))

  ;; This is called when the overlay is believed to be holding an RTL.
  ;; The overlay flag tells if it is incomplete new, or a fully formed RTL.
  ;; The contained extent literal is in quoted form.
  ;; The extent literal might be empty.
  (setq  
    RT-literal·describe-form_status 
    (append
      (list 
        'form-negative-overlay
        'too-small
        'not-quoted-form
        'null-literal
        )
      RT-literal·describe-new-form_status
      RT-literal·describe-existing-form_status
      ))
  (defun RT-literal·describe-form_has-error (status)
    (if 
      (memq status '(form-negative-overlay too-small not-quoted-form))
      t
      (if 
        (RT-literal·describe-new-form_has-error status)
        t
        (RT-literal·describe-existing-form_has-error status)
        )))
  (defun RT-literal·describe-form_message (status)
    (pcase status
      ('form-negative-overlay
        (message "RT-literal·describe-form:: overlay presents with negative size!")
        )
      ('too-small
        (message "RT-literal·describe-form:: too small to be in quoted form")
        )
      ('not-quoted-form
        (message "RT-literal·describe-form:: is not quoted form")
        )
      (_
        (progn
          (RT-literal·describe-new-form_message status)
          (RT-literal·describe-existing-form_message status)
          ))))
  (defun RT-literal·describe-form (overlay)
    (let 
      ( 
        (overlay_leftmost (RT-literal·overlay_leftmost overlay))
        (overlay_rightmost_right-neighbor (RT-literal·overlay_rightmost_right-neighbor overlay)) 
        )
      (let
        (
          (overlay-size (- (position-bytes overlay_rightmost_right-neighbor) (position-bytes overlay_leftmost)))
          )
        (if 
          (< overlay-size 0)
          (list 'form-negative-overlay)
          (if 
            (< overlay-size quote-pair_size)
            (list 'too-small)
            (if 
              (not (RT-literal·is-quoted-form overlay_leftmost overlay_rightmost_right-neighbor))
              (list 'not-quoted-form)
              (if 
                (= overlay-size quote-pair_size)
                (list 'null-literal)
                (let
                  (
                    (content_rightmost_right-neighbor (- overlay_rightmost_right-neighbor (length right-delimiter))) 
                    )
                  (if 
                    (overlay-get overlay 'is-new)
                    (RT-literal·describe-new-form overlay_leftmost content_rightmost_right-neighbor)
                    (RT-literal·describe-existing-form overlay_leftmost content_rightmost_right-neighbor)
                    )))))))))

  ;; Guard Logic
  ;;

  (defun RT-literal·editor·guard-modification (overlay after-p_bool modify_leftmost modify_rightmost_right-neighbor &optional length)
    (unless after-p_bool
      (let* (
          (is-strict_bool (overlay-get overlay 'RT-literal·is-strict))
          (overlay_leftmost (RT-literal·overlay_leftmost overlay))
          (overlay_rightmost_right-neighbor (RT-literal·overlay_rightmost_right-neighbor overlay))
          )
        (if 
          is-strict_bool
          ;; Active edit mode: block modifications that touch the delimiters or extent field
          (let 
            (
              (content_leftmost (overlay-get overlay 'RT-literal·content_leftmost))
              (content_rightmost_right-neighbor (- overlay_rightmost_right-neighbor (length right-delimiter)))
              )
            (when 
              (or (< modify_leftmost content_leftmost) (> modify_rightmost_right-neighbor content_rightmost_right-neighbor))
              (user-error "RT-literal error: Protected tape boundary. Exit edit mode first.")
              ))
          ;; Sealed mode: block everything unless engulfing
          (unless 
            (and (<= modify_leftmost overlay_leftmost) (>= modify_rightmost_right-neighbor overlay_rightmost_right-neighbor))
            (user-error "RT-literal error: Atomic tape. Select the entire literal to cut/delete.")
            )))))

  (defun RT-literal·editor·lock-overlay (overlay is-strict_bool)
    (overlay-put overlay 'RT-literal·is-strict is-strict_bool)
    (overlay-put overlay 'modification-hooks '(RT-literal·editor·guard-modification))
    )

  (defun RT-literal·editor·unlock-overlay (overlay)
    (overlay-put overlay 'RT-literal·is-strict nil)
    (overlay-put overlay 'modification-hooks nil)
    )

  ;; Internal wrapper logic
  ;;

  (defun RT-literal·editor·execute-wrap (payload_str start_pos end_pos msg_str)
    (let 
      (
        (extent (- (RT-literal·string-byte_count payload_str) 1))
        )
      (delete-region start_pos end_pos)
      (goto-char start_pos)
      (insert left-delimiter)
      (let 
        (
          (extent-field_str (format "%X " extent))
          )
        (insert extent-field_str)
        (let 
          (
            (content_leftmost (point))
            )
          (insert payload_str)
          (insert right-delimiter)
          (let 
            (
              (overlay (make-overlay start_pos (point) nil nil nil))
              )
            (overlay-put overlay 'RT-literal t)
            (overlay-put overlay 'RT-literal·content_leftmost (copy-marker content_leftmost))
            (RT-literal·editor·lock-overlay overlay nil)
            (RT-literal·apply-visibility overlay)
            (RT-literal·apply-display-wrap overlay)
            (deactivate-mark)
            (message msg_str)
            )))))

  ;; Display Indent (Wrap Prefix)
  ;;

  (defun RT-literal·apply-display-wrap (overlay)
    (if 
      (not RT-literal·display-wrap-enabled)
      (overlay-put overlay 'wrap-prefix nil)
      (let* (
          (start_pos (RT-literal·overlay_leftmost overlay))
          (col_num 
            (save-excursion
              (goto-char start_pos)
              (current-column)
              ))
          (total-indent (+ col_num RT-literal·display-wrap-offset))
          (prefix_str (make-string total-indent ?\s))
          )
        (overlay-put overlay 'wrap-prefix prefix_str)
        )))

  (defun RT-literal·set-display-wrap-all (enable_bool offset_num)
    (setq RT-literal·display-wrap-enabled enable_bool)
    (setq RT-literal·display-wrap-offset offset_num)
    (let 
      (
        (overlays_list (overlays-in (point-min) (point-max)))
        )
      (dolist (ov overlays_list)
        (when (overlay-get ov 'RT-literal)
          (RT-literal·apply-display-wrap ov)
          ))))

  ;; extent field visibility
  ;;

  (defun RT-literal·apply-visibility (overlay)
    (let 
      (
        (hide-ov (overlay-get overlay 'RT-literal·hide-overlay))
        (extent_start (+ (RT-literal·overlay_leftmost overlay) (length left-delimiter)))
        (extent_end (marker-position (overlay-get overlay 'RT-literal·content_leftmost)))
        )
      (if 
        (= extent_start extent_end)
        ;; Null literal, nothing to hide
        (when hide-ov
          (delete-overlay hide-ov)
          (overlay-put overlay 'RT-literal·hide-overlay nil)
          )
        (if 
          RT-literal·hide-extent
          (if 
            (not hide-ov)
            (let 
              (
                (new-ov (make-overlay extent_start extent_end nil nil nil))
                )
              (overlay-put new-ov 'invisible t)
              (overlay-put new-ov 'evaporate t)
              (overlay-put new-ov 'RT-literal·is-hide-overlay t)
              (overlay-put overlay 'RT-literal·hide-overlay new-ov)
              )
            (move-overlay hide-ov extent_start extent_end)
            )
          (when hide-ov
            (delete-overlay hide-ov)
            (overlay-put overlay 'RT-literal·hide-overlay nil)
            )))))

  (defun RT-literal·set-extent-field-visibility-all (hide_bool)
    (setq RT-literal·hide-extent hide_bool)
    (let 
      (
        (overlays_list (overlays-in (point-min) (point-max)))
        )
      (dolist (ov overlays_list)
        (when (overlay-get ov 'RT-literal)
          (RT-literal·apply-visibility ov)
          ))))

  ;; Edit Mode Shield
  ;;

  (defun RT-literal·insert-newline ()
    "Inserts a raw newline character, bypassing language formatting."
    (interactive)
    (insert "\n")
    )

  (defun RT-literal·insert-tab ()
    "Inserts a raw tab character, bypassing language formatting."
    (interactive)
    (insert "\t")
    )

  (defvar RT-literal-edit-mode-map
    (let 
      (
        (map (make-sparse-keymap))
        )
      (define-key map (kbd "RET") 'RT-literal·insert-newline)
      (define-key map (kbd "TAB") 'RT-literal·insert-tab)
      map
      ))

  (define-minor-mode RT-literal-edit-mode
    "Minor mode active while editing an RT-literal to prevent auto-formatting."
    :init-value nil
    :lighter " RT-Edit"
    :keymap RT-literal-edit-mode-map
    )

;;;------------------------------------------------------------------------------
;;; Interface (API)
;;;

  (defun RT-literal·editor·make ()
    (let 
      ( 
        (leftmost_pos (point))
        )
      (insert left-delimiter right-delimiter)
      (let 
        (
          (overlay (make-overlay leftmost_pos (point) nil nil nil)) 
          )
        (overlay-put overlay 'RT-literal t)
        (overlay-put overlay 'is-new t)
        (overlay-put overlay 'RT-literal·content_leftmost (copy-marker (+ leftmost_pos (length left-delimiter))))
        (RT-literal·apply-highlight overlay)
        (RT-literal·editor·lock-overlay overlay t)
        (RT-literal·stack-push RT-literal·overlay-stack overlay)
        (backward-char (length right-delimiter))
        (add-hook 'post-command-hook 'RT-literal·boundary-check nil t)
        (RT-literal-edit-mode 1)
        (message "New literal mode active. Type content and exit.")
        )))

  (defun RT-literal·editor·select-cycle ()
    (let* (
        (all-overlays_list (overlays-at (point)))
        (target-overlays_list nil)
        )
      (dolist (ov all-overlays_list)
        (when (overlay-get ov 'RT-literal)
          (push ov target-overlays_list)
          ))
      (if 
        (not target-overlays_list)
        (progn
          (when RT-literal·selected-overlay
            (RT-literal·apply-highlight RT-literal·selected-overlay)
            (setq RT-literal·selected-overlay nil)
            )
          (message "No literals at point.")
          )
        (let 
          (
            (current-index (RT-literal·list-index RT-literal·selected-overlay target-overlays_list))
            )
          (if 
            (not current-index)
            (progn
              (when RT-literal·selected-overlay
                (RT-literal·apply-highlight RT-literal·selected-overlay)
                )
              (setq RT-literal·selected-overlay (nth 0 target-overlays_list))
              (RT-literal·apply-selection RT-literal·selected-overlay)
              (message "Literal selected.")
              )
            (progn
              (RT-literal·apply-highlight RT-literal·selected-overlay)
              (if 
                (= current-index (- (length target-overlays_list) 1))
                (progn
                  (setq RT-literal·selected-overlay nil)
                  (message "Selection cleared.")
                  )
                (progn
                  (setq RT-literal·selected-overlay (nth (+ current-index 1) target-overlays_list))
                  (RT-literal·apply-selection RT-literal·selected-overlay)
                  (message "Next literal selected.")
                  ))))))))

  (defun RT-literal·editor·edit ()
    (if 
      (not (and RT-literal·selected-overlay (memq RT-literal·selected-overlay (overlays-at (point)))))
      (user-error "RT-literal error: You must select a literal at point first.")
      (let* (
          (target-overlay RT-literal·selected-overlay)
          (form (RT-literal·describe-form target-overlay))
          (status (car form))
          )
        (if 
          (RT-literal·describe-form_has-error status)
          (user-error "RT-literal error: Cannot edit, literal is structurally corrupt.")
          (progn
            (setq RT-literal·selected-overlay nil)
            (RT-literal·editor·unlock-overlay target-overlay)
            (RT-literal·apply-highlight target-overlay)
            (let 
              (
                (content_leftmost 
                  (pcase status
                    ('new-good (nth 1 form))
                    ('edited-good (nth 2 form))
                    ('null-literal (+ (RT-literal·overlay_leftmost target-overlay) (length left-delimiter)))
                    ('edited-null-content (nth 2 form))
                    ))
                )
              (overlay-put target-overlay 'RT-literal·content_leftmost (copy-marker content_leftmost))
              (RT-literal·editor·lock-overlay target-overlay t)
              )
            (RT-literal·stack-push RT-literal·overlay-stack target-overlay)
            (add-hook 'post-command-hook 'RT-literal·boundary-check nil t)
            (RT-literal-edit-mode 1)
            (message "Literal mode active. Type content and exit.")
            )))))

  ;; Returns 't' if successful, 'nil' if the form has an error.
  (defun RT-literal·editor·exit-no-pop (top-overlay)
    (let*
      (
        (form (RT-literal·describe-form top-overlay))
        (status (car form))
        )
      (if 
        (RT-literal·describe-form_has-error status)
        (progn
          (RT-literal·describe-form_message status)
          nil
          )
        (let 
          (
            (overlay_leftmost (RT-literal·overlay_leftmost top-overlay))
            (overlay_rightmost_right-neighbor (RT-literal·overlay_rightmost_right-neighbor top-overlay))
            (content_leftmost_marker (overlay-get top-overlay 'RT-literal·content_leftmost))
            )
          (let 
            (
              (content_leftmost (marker-position content_leftmost_marker))
              (inner_leftmost (+ overlay_leftmost (length left-delimiter)))
              (content_rightmost_right-neighbor (- overlay_rightmost_right-neighbor (length right-delimiter)))
              )
            (let 
              (
                (payload_str (buffer-substring-no-properties content_leftmost content_rightmost_right-neighbor))
                )
              (let
                (
                  (inhibit-read-only t)
                  )
                (RT-literal·editor·unlock-overlay top-overlay)
                (delete-region inner_leftmost content_rightmost_right-neighbor)
                (if 
                  (> (length payload_str) 0)
                  (let* (
                      (extent (- (RT-literal·string-byte_count payload_str) 1))
                      (extent-field_str (format "%X " extent))
                      )
                    (save-excursion
                      (goto-char inner_leftmost)
                      (insert extent-field_str)
                      (insert payload_str)
                      )
                    (set-marker content_leftmost_marker (+ inner_leftmost (length extent-field_str)))
                    )
                  (set-marker content_leftmost_marker inner_leftmost)
                  )
                (overlay-put top-overlay 'is-new nil)
                (RT-literal·editor·lock-overlay top-overlay nil)
                (overlay-put top-overlay 'face nil)
                (RT-literal·apply-visibility top-overlay)
                (RT-literal·apply-display-wrap top-overlay)
                t
                )))))))

  (defun RT-literal·editor·exit ()
    (if 
      (RT-literal·overlay-stack-is-empty)
      (user-error "RT-literal error: Not editing a literal.")
      (let 
        (
          (top-overlay (RT-literal·stack-top RT-literal·overlay-stack))
          )
        (if 
          (RT-literal·editor·exit-no-pop top-overlay)
          (let 
            (
              (final-cursor_pos (+ (RT-literal·overlay_rightmost_right-neighbor top-overlay) 1))
              )
            (RT-literal·stack-pop RT-literal·overlay-stack)
            (if 
              (RT-literal·overlay-stack-is-empty)
              (progn
                (remove-hook 'post-command-hook 'RT-literal·boundary-check t)
                (RT-literal-edit-mode -1)
                ))
            (goto-char final-cursor_pos)
            (message "Literal sealed and exited.")
            )))))

  (defun RT-literal·editor·abort ()
    (if 
      (RT-literal·overlay-stack-is-empty)
      (user-error "RT-literal error: Not editing a literal.")
      (let 
        (
          (top-overlay (RT-literal·stack-top RT-literal·overlay-stack))
          )
        (RT-literal·editor·unlock-overlay top-overlay)
        (let
          (
            (inhibit-read-only t)
            )
          (if 
            (overlay-get top-overlay 'is-new)
            (progn
              (delete-region 
                (RT-literal·overlay_leftmost top-overlay) 
                (RT-literal·overlay_rightmost_right-neighbor top-overlay)
                )
              (delete-overlay top-overlay)
              )
            (progn
              (overlay-put top-overlay 'face nil)
              ))
          (RT-literal·stack-pop RT-literal·overlay-stack)
          (if 
            (RT-literal·overlay-stack-is-empty)
            (progn
              (remove-hook 'post-command-hook 'RT-literal·boundary-check t)
              (RT-literal-edit-mode -1)
              ))
          (message "Literal editing aborted.")
          ))))

  ;; verifies boundaries; exits dynamically nested contexts if cursor leaves bounds
  (defun RT-literal·boundary-check ()
    (unless (RT-literal·overlay-stack-is-empty)
      (let 
        ( 
          (current_pos (point)) 
          )
        (while 
          (and 
            (not (RT-literal·overlay-stack-is-empty)) 
            (let 
              ( 
                (top-overlay (RT-literal·stack-top RT-literal·overlay-stack)) 
                )
              (or 
                (< current_pos (RT-literal·overlay_leftmost top-overlay)) 
                (> current_pos (RT-literal·overlay_rightmost_right-neighbor top-overlay))
                )))
          (let 
            (
              (top-overlay (RT-literal·stack-top RT-literal·overlay-stack))
              )
            (RT-literal·editor·exit-no-pop top-overlay)
            (RT-literal·stack-pop RT-literal·overlay-stack)
            (if 
              (RT-literal·overlay-stack-is-empty)
              (progn
                (remove-hook 'post-command-hook 'RT-literal·boundary-check t)
                (RT-literal-edit-mode -1)
                (message "Literal mode deactivated.")
                )))))))

  (defun RT-literal·scan-buffer ()
    (remove-overlays (point-min) (point-max) 'RT-literal t)
    (remove-overlays (point-min) (point-max) 'RT-literal·is-hide-overlay t)
    (let 
      (
        (overlay-stack nil)
        )
      (save-excursion
        (goto-char (point-min))
        (while (search-forward left-delimiter nil t)
          (let 
            (
              (leftmost_pos (match-beginning 0))
              )
            (if 
              (looking-at (regexp-quote right-delimiter))
              (let* (
                  (rightmost_right-neighbor (match-end 0))
                  (overlay (make-overlay leftmost_pos rightmost_right-neighbor nil nil nil))
                  (content_leftmost (match-beginning 0))
                  )
                (overlay-put overlay 'RT-literal t)
                (overlay-put overlay 'RT-literal·content_leftmost (copy-marker content_leftmost))
                (RT-literal·stack-push overlay-stack overlay)
                (goto-char rightmost_right-neighbor)
                )
              (when (looking-at "[0-9a-fA-F]+ ")
                (let* (
                    (content_leftmost (match-end 0))
                    (extent_str (match-string 0))
                    (extent (string-to-number extent_str 16))
                    (content-size (+ extent 1))
                    (content_rightmost_right-neighbor (byte-to-position (+ (position-bytes content_leftmost) content-size)))
                    )
                  (when content_rightmost_right-neighbor
                    (goto-char content_rightmost_right-neighbor)
                    (when (looking-at (regexp-quote right-delimiter))
                      (let* (
                          (rightmost_right-neighbor (match-end 0))
                          (overlay (make-overlay leftmost_pos rightmost_right-neighbor nil nil nil))
                          )
                        (overlay-put overlay 'RT-literal t)
                        (overlay-put overlay 'RT-literal·content_leftmost (copy-marker content_leftmost))
                        (RT-literal·stack-push overlay-stack overlay)
                        ))
                    (goto-char content_leftmost)
                    )))))))
      (while overlay-stack
        (let 
          (
            (overlay (RT-literal·stack-pop overlay-stack))
            )
          (RT-literal·editor·lock-overlay overlay nil)
          (RT-literal·apply-visibility overlay)
          (RT-literal·apply-display-wrap overlay)
          ))))

  (defun RT-literal·teardown ()
    (while (not (RT-literal·overlay-stack-is-empty))
      (RT-literal·editor·abort)
      )
    (remove-overlays (point-min) (point-max) 'RT-literal t)
    (remove-overlays (point-min) (point-max) 'RT-literal·is-hide-overlay t)
    (remove-hook 'post-command-hook 'RT-literal·boundary-check t)
    (setq RT-literal·selected-overlay nil)
    )

  (defun RT-literal·nuke-zombies ()
    (remove-overlays (point-min) (point-max) 'RT-literal t)
    (remove-overlays (point-min) (point-max) 'RT-literal·is-hide-overlay t)
    (remove-overlays (point-min) (point-max) 'read-only t)
    (remove-overlays (point-min) (point-max) 'left-prot t)
    (remove-overlays (point-min) (point-max) 'right-prot t)
    )

  (defun RT-literal·quote-region ()
    (if 
      (not (use-region-p))
      (user-error "RT-literal error: No active region to quote.")
      (let* (
          (start_pos (region-beginning))
          (end_pos (region-end))
          (raw_str (buffer-substring-no-properties start_pos end_pos))
          )
        (RT-literal·editor·execute-wrap raw_str start_pos end_pos "Region wrapped into sealed extent literal.")
        )))

  (defun RT-literal·convert-region ()
    (if 
      (not (use-region-p))
      (user-error "RT-literal error: No active region to convert.")
      (let* (
          (start_pos (region-beginning))
          (end_pos (region-end))
          (raw_str (buffer-substring-no-properties start_pos end_pos))
          (stripped_str (RT-literal·strip-outer-quotes raw_str))
          (payload_str (RT-literal·unescape-string stripped_str))
          )
        (RT-literal·editor·execute-wrap payload_str start_pos end_pos "Region converted and sealed as extent literal.")
        )))

  (defun RT-literal·disable-auto-scan ()
    (dolist (hook_sym RT-literal·target-mode-hooks_list)
      (remove-hook hook_sym 'RT-literal·scan-buffer)
      ))

;;;------------------------------------------------------------------------------
;;; Interactive (Commands)
;;;

  (defun RT-literal·editor·make_cmd ()
    (interactive)
    (RT-literal·editor·make)
    )

  (defun RT-literal·editor·select-cycle_cmd ()
    (interactive)
    (RT-literal·editor·select-cycle)
    )

  (defun RT-literal·editor·edit_cmd ()
    (interactive)
    (RT-literal·editor·edit)
    )

  (defun RT-literal·editor·exit_cmd ()
    (interactive)
    (RT-literal·editor·exit)
    )

  (defun RT-literal·editor·abort_cmd ()
    (interactive)
    (RT-literal·editor·abort)
    )

  (defun RT-literal·scan-buffer_cmd ()
    "Scans the buffer for valid extent literals, applying read-only overlays."
    (interactive)
    (RT-literal·scan-buffer)
    (message "Buffer scanned and extent literals sealed.")
    )

  (defun RT-literal·teardown_cmd ()
    "Completely removes RT-literal functionality, locks, and visuals from the current buffer."
    (interactive)
    (RT-literal·teardown)
    (message "RT-literal·teardown complete. All extent-liters should now be clear text. RT-literal·scan-buffer_cmd to rescan the buffer, or if it is auto-scan hooked, reload it.")
    )

  (defun RT-literal·nuke-zombies_cmd ()
    "Aggressively purges test overlays from the buffer."
    (interactive)
    (RT-literal·nuke-zombies)
    (message "RT-literal·nuke-zombies complete, all overlays should be gone.")
    )

  (defun RT-literal·quote-region_cmd ()
    "Converts an active region of raw text into a sealed extent literal."
    (interactive)
    (RT-literal·quote-region)
    )

  (defun RT-literal·convert-region_cmd ()
    "Strips outer quotes, unescapes characters, and converts region into a sealed extent literal."
    (interactive)
    (RT-literal·convert-region)
    )

  (defun RT-literal·extent-field-hide_cmd ()
    "Explicitly hides the extent fields for all literals in the buffer."
    (interactive)
    (RT-literal·set-extent-field-visibility-all t)
    (message "Extent fields hidden.")
    )

  (defun RT-literal·extent-field-show_cmd ()
    "Explicitly shows the extent fields for all literals in the buffer."
    (interactive)
    (RT-literal·set-extent-field-visibility-all nil)
    (message "Extent fields visible.")
    )

  (defun RT-literal·disable-auto-scan_cmd ()
    "Removes the auto-scan hook from all targeted major modes."
    (interactive)
    (RT-literal·disable-auto-scan)
    (message "RT-literal auto-scan disabled globally.")
    )

  (defun RT-literal·display-wrap-on_cmd (&optional prefix_arg)
    "Enables display-only soft wrap indenting for extent literals."
    (interactive "P")
    (let 
      (
        (offset (if prefix_arg (prefix-numeric-value prefix_arg) 2))
        )
      (RT-literal·set-display-wrap-all t offset)
      (message "Display wrap enabled with offset %d." offset)
      ))

  (defun RT-literal·display-wrap-off_cmd ()
    "Disables display-only soft wrap indenting for extent literals."
    (interactive)
    (RT-literal·set-display-wrap-all nil 0)
    (message "Display wrap disabled.")
    )

;;;--------------------------------------------------------------------------------
;;; Integration
;;;

  (defun RT-literal·enable-auto-scan ()
    (dolist (hook_sym RT-literal·target-mode-hooks_list)
      (add-hook hook_sym 'RT-literal·scan-buffer)
      ))

  (RT-literal·enable-auto-scan)

  (defvar RT-literal-mode-map
    (let 
      (
        (map (make-sparse-keymap))
        )
      (define-key map (kbd "m") 'RT-literal·editor·make_cmd)
      (define-key map (kbd "e") 'RT-literal·editor·edit_cmd)
      (define-key map (kbd "s") 'RT-literal·editor·select-cycle_cmd)
      (define-key map (kbd "a") 'RT-literal·editor·abort_cmd)
      (define-key map (kbd "x") 'RT-literal·editor·exit_cmd)
      (define-key map (kbd "q") 'RT-literal·quote-region_cmd)
      (define-key map (kbd "c") 'RT-literal·convert-region_cmd)
      ;; [o]ptions
      (define-key map (kbd "o e 1") 'RT-literal·extent-field-show_cmd)
      (define-key map (kbd "o e 0") 'RT-literal·extent-field-hide_cmd)
      (define-key map (kbd "o i 1") 'RT-literal·display-wrap-on_cmd)
      (define-key map (kbd "o i 0") 'RT-literal·display-wrap-off_cmd)
      map
      ))

  (global-set-key (kbd "M-o") RT-literal-mode-map)
