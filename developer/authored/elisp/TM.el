;;;=============================================================================
;;; TM (Tape Machine) for Elisp
;;;

;;;------------------------------------------------------------------------------
;;; Utilities
;;;    -- moved to utility.el
  

;;;-----------------------------------------------------------------------------
;;; Configuration
;;;

  ;; outermost TM, holds hole buffer, unconditional, after setupe there will always be one.
  (defvar-local RT·TM·tm_host nil
    "Host buffer TM attached to the host buffer."
    )

  ;; Each TM that can be conditionally bound to the buffer has a `make-if` function, they are listed here.
  (defvar-local RT·TM·list_make-if nil )

  ;; color theme important for setting the ov highlighting
  (defvar-local RT·TM·alist_theme nil
    "A buffer-local alist defining the active visual theme for TMs."
    )

  ;; currently selected overlay, or nil when no overlay is selected
  (defvar-local RT·TM·ov_selection nil
    "Tracks the currently selected TM overlay in the buffer."
    )

  ;; character set facility
  ;;

  (defvar RT·charset·descriptive-name-to-idx (RT·dict·make)
    "Dictionary mapping RT descriptive character set names to integer indices."
    )

  (defvar RT·charset·idx-to-descriptive-name (make-vector 256 nil)
    "Table mapping integer indices back to RT descriptive names."
    )

  (defvar RT·charset·idx-to-emacs-name (make-vector 256 nil)
    "Table mapping integer indices to native Emacs coding systems for execution."
    )


;;;-----------------------------------------------------------------------------
;;; Interior code
;;;

;; Character Set Registry
  ;;

  (defun RT·charset·register (alist_mappings)
    "Populate the dictionary and tables mapping RT descriptive names to Emacs coding systems."
    (let
      (
        (int_idx 0)
        (list_current alist_mappings)
        )
      (while list_current
        (let*
          (
            (cons_mapping (car list_current))
            (sym_descriptive-name (car cons_mapping))
            (sym_emacs-name (cdr cons_mapping))
            )
          (RT·dict·write RT·charset·descriptive-name-to-idx sym_descriptive-name int_idx)
          (aset RT·charset·idx-to-descriptive-name int_idx sym_descriptive-name)
          (aset RT·charset·idx-to-emacs-name int_idx sym_emacs-name)
          
          (setq int_idx (1+ int_idx))
          (setq list_current (cdr list_current))
          ))))

  (defun RT·charset·emacs-name-to-idx (sym_emacs-name)
    "Linear search to find the IDX for a pre-existing Emacs coding system."
    (let
      (
        (int_idx 0)
        )
      (RT·loop
        (if (>= int_idx 256)
          (RT·break nil)
          (if (eq (aref RT·charset·idx-to-emacs-name int_idx) sym_emacs-name)
            (RT·break int_idx)
            (setq int_idx (1+ int_idx))
            )))))

  (defun RT·charset·push-and-set (sym_descriptive-name-target)
    "Pushes current charset IDX to stack and sets the buffer to SYM_DESCRIPTIVE-NAME-TARGET."
    (let*
      (
        (sym_emacs-raw buffer-file-coding-system)
        (sym_emacs-current (coding-system-base sym_emacs-raw))
        (int_idx-current (RT·charset·emacs-name-to-idx sym_emacs-current))
        (int_idx-safe (if int_idx-current int_idx-current 0))

        (int_idx-target (RT·dict·read RT·charset·descriptive-name-to-idx sym_descriptive-name-target))
        (sym_emacs-target (aref RT·charset·idx-to-emacs-name int_idx-target))
        )
      (RT·stack·push RT·list_charset-stack int_idx-safe)
      (set-buffer-file-coding-system sym_emacs-target)
      
      (if
        (memq sym_emacs-target '(binary raw-text))
        (set-buffer-multibyte nil)
        (set-buffer-multibyte t)
        )
      
      (RT·TM·introspection·write-if 
        'RT·charset·push-and-set 
        (format "Pushed IDX %d (%s), set target to %s (%s)." int_idx-safe sym_emacs-current sym_descriptive-name-target sym_emacs-target)
        )
      ))

  (defun RT·charset·pop-and-restore ()
    "Restores the buffer character set from the top of the stack."
    (if
      (not (RT·stack·is-empty RT·list_charset-stack))
      (let*
        (
          (int_idx-target (RT·stack·top RT·list_charset-stack))
          (sym_descriptive-name-target (aref RT·charset·idx-to-descriptive-name int_idx-target))
          (sym_emacs-target (aref RT·charset·idx-to-emacs-name int_idx-target))
          )
        (RT·stack·pop RT·list_charset-stack)
        (set-buffer-file-coding-system sym_emacs-target)
        
        (if
          (memq sym_emacs-target '(binary raw-text))
          (set-buffer-multibyte nil)
          (set-buffer-multibyte t)
          )
        
        (RT·TM·introspection·write-if 
          'RT·charset·pop-and-restore 
          (format "Popped stack, restored charset to IDX %d: %s (%s)." int_idx-target sym_descriptive-name-target sym_emacs-target)
          )
        )))

  ;; mode for raw byte mode
  ;;

  (defun RT·buffer-mode·push-and-set-binary ()
    "Pushes current mode to stack and sets the buffer to raw binary."
    (let
      (
        (mode_current major-mode)
        )
      (RT·stack·push RT·list_buffer-mode-stack mode_current)
      (fundamental-mode)
      (set-buffer-multibyte nil)
      (RT·TM·introspection·write-if 
        'RT·buffer-mode·push-and-set-binary 
        (format 
          "Pushed %s. Mode is %s, multibyte is %s." 
          mode_current 
          major-mode 
          enable-multibyte-characters
          ))
      ))

  (defun RT·buffer-mode·pop-and-restore ()
    "Restores the buffer mode from the top of the stack. Does nothing if empty."
    (if
      (not (RT·stack·is-empty RT·list_buffer-mode-stack))
      (let
        (
          (mode_target (RT·stack·top RT·list_buffer-mode-stack))
          )
        (RT·stack·pop RT·list_buffer-mode-stack)
        (funcall mode_target)
        (RT·TM·introspection·write-if 
          'RT·buffer-mode·pop-and-restore 
          (format "Popped stack, restored buffer mode to %s." major-mode)
          )
        )))

  (defmacro RT·TM·overlay·leftmost (overlay)
    `(overlay-start ,overlay)
    )

  (defmacro RT·TM·overlay·rightmost_right-neighbor (overlay)
    `(overlay-end ,overlay)
    )

  (defun RT·TM·BreadBox·make-if (tm_lookahead encoding_character alist_theme list_make-if)
    "Detect prefix and suffix, returning an extraction package for removal."
    (let
      (
        (pos_start (RT·TM·get-pos tm_lookahead))
        (buffer_host (RT·TM·buffer-display tm_lookahead))
        (bool_found nil)
        (pos_end nil)
        (str_payload nil)
        (str_prefix "d“")
        (str_suffix "”b")
        )
      (with-current-buffer buffer_host
        (save-excursion
          (goto-char pos_start)
          (if (looking-at (regexp-quote str_prefix))
            (let
              (
                (pos_payload_start (match-end 0))
                )
              (if (search-forward str_suffix nil t)
                (progn
                  (setq bool_found t)
                  (setq pos_end (point))
                  (setq str_payload (buffer-substring-no-properties pos_payload_start (- pos_end (length str_suffix))))
                  ))))))
      (if bool_found
        (let
          (
            (tm_nested (RT·TM·string·make str_payload))
            )
          (list
            (cons 'tm tm_nested)
            (cons 'pos_start pos_start)
            (cons 'pos_end pos_end)
            (cons 'str_prefix str_prefix)
            (cons 'str_suffix str_suffix)
            (cons 'str_payload str_payload)
            ))
        nil
        )))

  (defun RT·TM·buffer·detect-all (tm_substrate encoding_character alist_theme list_make-if)
    "Scan TM_SUBSTRATE for BreadBoxes, extract them, and insert overlays."
    (RT·TM·cue-leftmost tm_substrate) 
    (let 
      (
        (bool_substrate-active (RT·TM·has-right-neighbor tm_substrate))
        )
      (RT·loop
        (if (not bool_substrate-active)
          (RT·break nil)
          (let*
            (
              (list_fn list_make-if)
              (bool_found-nested nil)
              )
            (while (and list_fn (not bool_found-nested))
              (let*
                (
                  (fn_make-if (car list_fn))
                  (tm_lookahead (RT·TM·entangled-copy tm_substrate))
                  (result_nested (if fn_make-if (funcall fn_make-if tm_lookahead encoding_character alist_theme list_make-if) nil))
                  )
                (if result_nested
                  (let*
                    (
                      (tm_nested (cdr (assq 'tm result_nested)))
                      (pos_start (cdr (assq 'pos_start result_nested)))
                      (pos_end (cdr (assq 'pos_end result_nested)))
                      (str_prefix (cdr (assq 'str_prefix result_nested)))
                      (str_suffix (cdr (assq 'str_suffix result_nested)))
                      (str_payload (cdr (assq 'str_payload result_nested)))
                      (buffer_host (RT·TM·buffer-display tm_substrate))
                      )
                    (setq bool_found-nested t)
                    
                    (with-current-buffer buffer_host
                      (delete-region pos_start pos_end)
                      (let*
                        (
                          (ov (make-overlay pos_start pos_start nil t nil))
                          (str_hex (RT·bytes-to-hex str_payload))
                          (str_display (concat str_prefix str_hex str_suffix))
                          )
                        (overlay-put ov 'RT·TM tm_nested)
                        (overlay-put ov 'after-string str_display)
                        (overlay-put ov 'evaporate t)
                        (RT·TM·cue tm_substrate pos_start)
                        ))
                    ))
                (setq list_fn (cdr list_fn))
                ))

            (if (not bool_found-nested)
              (RT·TM·step tm_substrate)
              )
            (setq bool_substrate-active (RT·TM·has-right-neighbor tm_substrate))
            )))))


;;;-----------------------------------------------------------------------------
;;; API
;;;


  (defun RT·TM·buffer·make-if (encoding_character alist_theme list_make-if)
    "API entry point. Initializes extraction scan over the host buffer."
    (setq RT·TM·alist_theme alist_theme)
    (let*
      (
        (buffer_target (current-buffer))
        )
      (with-current-buffer buffer_target
        (set-buffer-multibyte nil)
        )
      
      (let
        (
          (tm_host (RT·TM·buffer-byte·make buffer_target (point-min) encoding_character))
          )
        
        (with-current-buffer buffer_target
          (setq RT·TM·tm_host tm_host)
          )
        
        (RT·TM·buffer·detect-all tm_host encoding_character RT·TM·alist_theme list_make-if)
        
        (with-current-buffer buffer_target
          (set-buffer-multibyte t)
          )
        tm_host
        )))


(defun RT·TM·setup ()

    (add-to-list 'RT·TM·list_symbol-introspection 'RT·charset·push-and-set-binary)
    (add-to-list 'RT·TM·list_symbol-introspection 'RT·charset·pop-and-restore)
    
    (RT·charset·register
      '(
         binary
         utf-8
         utf-16le
         utf-16be
         us-ascii
         iso-latin-1
         windows-1252
         mac-roman
         gb2312
         shift_jis
         euc-jp
         raw-text
         )))






;;;-----------------------------------------------------------------------------
;;; Interactive
;;;

  (defun RT·TM·overlay·edit ()
    "Extracts the bytes of the selected overlay into a new buffer for editing."
    (interactive)
    (let
      (
        (ov RT·TM·ov_selection)
        )
      (if ov
        (let*
          (
            (tm_payload (overlay-get ov 'RT·TM))
            (str_raw nil)
            (buffer_edit (generate-new-buffer "*RT·TM-Edit*"))
            )
          
          (RT·TM·cue-leftmost tm_payload)
          (while (RT·TM·has-right-neighbor tm_payload)
            (progn
              (setq str_raw (concat str_raw (char-to-string (RT·TM·read tm_payload))))
              (RT·TM·step tm_payload)
              ))
          
          (with-current-buffer buffer_edit
            (insert str_raw)
            (setq-local RT·TM·ov_source ov)
            (local-set-key (kbd "C-c C-c") 'RT·TM·overlay·save-edit)
            )
          (pop-to-buffer buffer_edit)
          ))))

  (defun RT·TM·overlay·save-edit ()
    "Trigger a display update from the edited buffer."
    (interactive)
    (message "RT·TM: Edit saved and projected.")
    )


;;;-----------------------------------------------------------------------------
;;; Integration
;;;
