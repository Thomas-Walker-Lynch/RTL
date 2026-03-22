;;;--------------------------------------------------------------------------------
;;; An extent literal is embedded in the document with this syntax:
;;;
;;;   literal embedding:: 
;;;     <left_delimiter><extent_field><content_field><right_delimiter> | <left delimiter><right delimiter>
;;;
;;;       extent_field::
;;;         <hex encode unsigned integer extent><space>
;;;
;;;       content_field::
;;;         array with max index of said extent of anything
;;;
;;; It is displayed to the user with this syntax:
;;;   literal display:: 
;;;     <left_delimiter>[<content_field>]<right_delimiter>
;;;
;;; The command to enter a new literal is:
;;;
;;;         M-x RT-literal·editor·make
;;;         M-x RT-literal·editor·exit
;;;
;;;     if cdot, ·, causes difficulties, consider using a hot key. 
;;;     See the Interface and Keybindings sections toward the bottom for more commands.
;;;
;;; --------------------------------------------------------------------------------
;;; TTCA Theory vs. Emacs:
;;;
;;; In TTCA, a tape is an array of cells. A tape has a leftmost cell and a rightmost cell. Hence rightmost_right-neighbor is one cell to the right of rightmost. A tape area is not guaranteed to have a rightmost_right-neighbor, but it will always have a rightmost. The index of the leftmost cell is conventionally 0, the index of the rightmost cell is called the extent of the tape.  
;;;
;;; In C programming culture terminology 'length' is a count of cells in an array (a kind of tape). Hence length is 1 plus extent, and can overflow an index register, whereas the 'extent' is an index, and defines the required bit size of an index register.  'size' in C culture programming convention had the meaning of a count of the underlying bytes, unless otherwise noted. When speaking of arrays of bytes, 'extent' is the maximum byte index.
;;;
;;; In the context of this code, and when discussing extent literals, 'extent' refers to the byte extent.
;;;
;;; --------------------------------------------------------------------------------
;;; We also have a conflict in interpretation of 'position' between position in Emacs and indexes in TTCA theory.  A tape (array) has cells, and an index (~ cursor position) indicates a cell.  Leftmost is then the leftmost cell, while rightmost is the rightmost cell.  
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
  (defvar-local RT-literal·hide-extent t)

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

  (defun RT-literal·enable-auto-scan ()
    (dolist (hook_sym RT-literal·target-mode-hooks_list)
      (add-hook hook_sym 'RT-literal·scan-buffer)
      ))

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



  ;;
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

  ;; By contract: only called when overlay holds a newly made, non-null, extent-literal.
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

  ;; This is called when the overlay is believed to be holding an extent-literal.
  ;; The overlay flag tells if it is incomplete new, or a fully formed extent-literal.
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

  ;; extent field as a first class citizen
  ;;

  (defun RT-literal·extent-remove (extent_pos extent-field-right-neighbor_pos)
    (delete-region extent_pos extent-field-right-neighbor_pos)
    )

  (defun RT-literal·extent-insert (extent_pos extent)
    (when (> extent 0)
      (save-excursion
        (goto-char extent_pos)
        (insert (format "%X " extent))
        )))

  (defun RT-literal·extent-update (extent_pos extent-field-right-neighbor_pos extent)
    (RT-literal·extent-remove extent_pos extent-field-right-neighbor_pos)
    (RT-literal·extent-insert extent_pos extent)
    )

  (defun RT-literal·extent-hide (extent_pos extent-field-right-neighbor_pos)
    (let 
      (
        (hide-overlay (make-overlay extent_pos extent-field-right-neighbor_pos nil nil nil))
        )
      (overlay-put hide-overlay 'invisible t)
      (overlay-put hide-overlay 'RT-literal·is-hidden-extent_bool t)
      ))

  (defun RT-literal·extent-show (extent_pos)
    (let 
      (
        (overlay_list (overlays-at extent_pos))
        )
      (dolist (overlay overlay_list)
        (when (overlay-get overlay 'RT-literal·is-hidden-extent_bool)
          (delete-overlay overlay)
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
                (message "Literal mode deactivated.")
                )))))))

  ;; Guard Logic
  ;;

  (defun RT-literal·editor·guard-modification (overlay after-p_bool modify_leftmost modify_rightmost_right-neighbor &optional length)
    (unless inhibit-read-only
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
              ))))))

  (defun RT-literal·editor·lock-overlay (overlay is-strict_bool)
    (overlay-put overlay 'RT-literal·is-strict is-strict_bool)
    (overlay-put overlay 'modification-hooks '(RT-literal·editor·guard-modification))
    )

  (defun RT-literal·editor·unlock-overlay (overlay)
    (overlay-put overlay 'RT-literal·is-strict nil)
    (overlay-put overlay 'modification-hooks nil)
    )

  ;; conversion to extent-literal
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
            (deactivate-mark)
            (message msg_str)
            )))))

;;;------------------------------------------------------------------------------
;;; Interactive Interface
;;;

  (defun RT-literal·teardown ()
    "Completely removes RT-literal functionality, locks, and visuals from the current buffer."
    (while (not (RT-literal·overlay-stack-is-empty))
      (RT-literal·editor·abort)
      )
    (remove-overlays (point-min) (point-max) 'RT-literal t)
    (remove-hook 'post-command-hook 'RT-literal·boundary-check t)
    (setq RT-literal·selected-overlay nil)
    )

  (defun RT-literal·teardown_cmd ()
    (interactive)
    (RT-literal·teardown)
    (message "RT-literal·teardown complete. All extent-liters should now be clear text. RT-literal·scan-buffer_cmd to rescan the buffer, or if it is auto-scan hooked, reload it.")
    )

  (defun RT-literal·nuke-zombies ()
    "Aggressively purges test overlays from the buffer."
    (remove-overlays (point-min) (point-max) 'RT-literal t)
    (remove-overlays (point-min) (point-max) 'read-only t)
    (remove-overlays (point-min) (point-max) 'left-prot t)
    (remove-overlays (point-min) (point-max) 'right-prot t)
    (message "Zombie overlays cleared. Ready for clean testing.")
    )

  (defun RT-literal·nuke-zombies_cmd ()
    (interactive)
    (RT-literal·nuke-zombies)
    (message "RT-literal·editor·nuke-zombies complete, all overlays should be gone.")
    )


  (defun RT-literal·disable-auto-scan ()
    (dolist (hook_sym RT-literal·target-mode-hooks_list)
      (remove-hook hook_sym 'RT-literal·scan-buffer)
      ))

  (defun RT-literal·disable-auto-scan_cmd ()
    "Removes the auto-scan hook from all targeted major modes."
    (interactive)
    (RT-literal·disable-auto-scan)
    (message "RT-literal auto-scan disabled globally.")
    )

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
        (message "New literal mode active. Type content and exit.")
        )))

  (defun RT-literal·editor·make_cmd ()
    (interactive)
    (RT-literal·editor·make)
    )

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

    (defun RT-literal·editor·select-cycle_cmd ()
    (interactive)
    (RT-literal·editor·select-cycle)
    )

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
            (message "Literal mode active. Type content and exit.")
            )))))


  (defun RT-literal·editor·edit_cmd ()
    (interactive)
    (RT-literal·editor·edit)
    )

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
                (delete-region inner_leftmost content_rightmost_right-neighbor)
                (if 
                  (> (length payload_str) 0)
                  (let 
                    (
                      (extent (- (RT-literal·string-byte_count payload_str) 1))
                      )
                    (save-excursion
                      (goto-char inner_leftmost)
                      (insert (format "%X " extent))
                      (insert payload_str)
                      )))
                (overlay-put top-overlay 'is-new nil)
                (RT-literal·editor·lock-overlay top-overlay nil)
                (overlay-put top-overlay 'face nil)
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
              (remove-hook 'post-command-hook 'RT-literal·boundary-check t)
              )
            (goto-char final-cursor_pos)
            (message "Literal sealed and exited.")
            )))))


  (defun RT-literal·editor·exit_cmd ()
    (interactive)
    (RT-literal·editor·exit)
    )

  (defun RT-literal·editor·abort ()
    (if 
      (RT-literal·overlay-stack-is-empty)
      (user-error "RT-literal error: Not editing a literal.")
      (let 
        (
          (top-overlay (RT-literal·stack-top RT-literal·overlay-stack))
          )
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
              (RT-literal·editor·lock-overlay top-overlay nil)
              (overlay-put top-overlay 'face nil)
              ))
          (RT-literal·stack-pop RT-literal·overlay-stack)
          (if 
            (RT-literal·overlay-stack-is-empty)
            (remove-hook 'post-command-hook 'RT-literal·boundary-check t)
            )
          (message "Literal editing aborted.")
          ))))

  (defun RT-literal·editor·abort_cmd ()
    (interactive)
    (RT-literal·editor·abort)
    )

  (defun RT-literal·scan-buffer ()
    (remove-overlays (point-min) (point-max) 'RT-literal t)
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
          ))))


  (defun RT-literal·scan-buffer_cmd ()
    "Scans the buffer for valid extent literals, applying read-only overlays."
    (interactive)
    (RT-literal·scan-buffer)
    (message "Buffer scanned and extent literals sealed.")
    )

  (defun RT-literal·editor·quote-region ()
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

  (defun RT-literal·editor·quote-region_cmd ()
    "Converts an active region of raw text into a sealed extent literal."
    (interactive)
    (RT-literal·editor·quote-region)
    )

  (defun RT-literal·editor·convert-region ()
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

  (defun RT-literal·editor·convert-region_cmd ()
    "Strips outer quotes, unescapes characters, and converts region into a sealed extent literal."
    (interactive)
    (RT-literal·editor·convert-region)
    )


;;;--------------------------------------------------------------------------------
;;; integration
;;;

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
      map
      ))
  (global-set-key (kbd "M-o") RT-literal-mode-map)
  ;;; (global-set-key (kbd "C-x \"") RT-literal-mode-map)
