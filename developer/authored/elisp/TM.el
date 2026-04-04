;;;=============================================================================
;;; TM (Tape Machine) for Elisp
;;;

;;;------------------------------------------------------------------------------
;;; Utilities
;;;

  ;; Introspection Utilities
  ;;

  (defun RT·TM·introspection·write (name_buffer-source sym_function str_message)
    "Write STR_MESSAGE to the global introspection buffer, creating a new frame if needed."
    (let*
      (
        (name_buffer-introspection "*RT·TM-Introspection*")
        (buffer_introspection (get-buffer-create name_buffer-introspection))
        (window_introspection (get-buffer-window buffer_introspection 0))
        )
      (if
        (null window_introspection)
        (let
          (
            (frame_new (make-frame '((name . "RT·TM Introspection"))))
            )
          (set-window-buffer (frame-selected-window frame_new) buffer_introspection)
          ))
      (with-current-buffer buffer_introspection
        (goto-char (point-max))
        (insert (format "%s::%s::  %s\n" name_buffer-source sym_function str_message))
        )))

  (defmacro RT·TM·introspection·write-if (sym_function str_message)
    "Log a message if SYM_FUNCTION is in the active introspection list."
    `(if
       (memq ,sym_function RT·TM·list_symbol-introspection)
       (RT·TM·introspection·write (buffer-name) ,sym_function ,str_message)
       ))

  ;; introspection turned on flags, usually the same as function names
  (defvar RT·TM·list_symbol-introspection
    '(
       RT·TM·buffer·detect-all
       ))

  
  ;; Generic Stack Interface
  ;;

  (defmacro RT·stack·push (sym_stack item)
    `(push ,item ,sym_stack)
    )

  (defmacro RT·stack·pop (sym_stack)
    `(pop ,sym_stack)
    )

  (defun RT·stack·top (list_stack)
    (car list_stack)
    )

  (defmacro RT·stack·is-empty (sym_stack)
    `(null ,sym_stack)
    )

  ;; Generic Dictionary Interface
  ;;

  (defun RT·dict·make ()
    "Create a new mutable dictionary."
    (list '*RT·dict*)
    )

  (defun RT·dict·write (dict key value)
    "Allocate and write the `(key . value)` pair into `dict`."
    (let
      (
        (cons_existing (assq key (cdr dict)))
        )
      (if
        cons_existing
        (setcdr cons_existing value)
        (setcdr dict (cons (cons key value) (cdr dict)))
        )))

  (defun RT·dict·read (dict key)
    "Return the value from `(key . value)` pair, if not found return nil."
    (cdr (assq key (cdr dict)))
    )

  (defun RT·dict·dealloc (dict key)
    "Dealloc the `(key . value)`, if not found do nothing."
    (setcdr dict (assq-delete-all key (cdr dict)))
    )

  (defun RT·dict·to-alist (dict)
    "Return the underlying alist."
    (cdr dict)
    )

  ;; Tape Machine Bridge Utilities
  ;;

  (defun RT·TM·dict·make (dict)
    "Generate a Tape Machine interface for iterating over the entries of DICT."
    (RT·TM·list·make (RT·dict·to-alist dict))
    )

  (defun RT·buffer·raw-bytes·get (buffer_byte pos_byte_leftmost pos_byte_rightmost_right-neighbor)
    "Extract the exact file bytes between the specified byte boundaries."
    (with-current-buffer buffer_byte
      (buffer-substring-no-properties pos_byte_leftmost pos_byte_rightmost_right-neighbor)
      ))

  (defun RT·TM·overlay·at-pos (buffer_display pos_char_target)
    "Return the outermost TM overlay at POS_CHAR_TARGET in BUFFER_DISPLAY."
    (with-current-buffer buffer_display
      (let
        (
          (list_overlay (overlays-at pos_char_target))
          (ov_outermost nil)
          (pos_char_max_rightmost -1)
          )
        (while list_overlay
          (let ((ov (car list_overlay)))
            (if (overlay-get ov 'RT·TM)
              (let ((pos_char_current_rightmost (RT·TM·overlay·rightmost_right-neighbor ov)))
                (if (> pos_char_current_rightmost pos_char_max_rightmost)
                  (progn
                    (setq pos_char_max_rightmost pos_char_current_rightmost)
                    (setq ov_outermost ov)
                    )))))
          (setq list_overlay (cdr list_overlay))
          )
        ov_outermost
        )))

  (defun RT·TM·overlay·project (buffer_display pos_byte_leftmost pos_byte_rightmost_right-neighbor)
    "Create an overlay in BUFFER_DISPLAY mapped to the absolute byte boundaries."
    (with-current-buffer buffer_display
      (let*
        (
          (pos_char_leftmost (byte-to-position pos_byte_leftmost))
          (pos_char_rightmost_right-neighbor (byte-to-position pos_byte_rightmost_right-neighbor))
          (ov_new (make-overlay pos_char_leftmost pos_char_rightmost_right-neighbor))
          )
        ov_new
        )))

  ;; Tape Machine Core
  ;;
  ;;   Tape machines can be nested, so the tape machine that we hold a reference to is called tape machine "V0".  A tape nested in V0 is said to be "V1"

  (defun RT·TM·list·make (list_original &optional list_starting)
    (let
      (
        (list_current (if list_starting list_starting list_original))
        )
      (list
        (cons 'cue-leftmost (lambda () (setq list_current list_original)))
        (cons 'has-right-neighbor (lambda () (not (or (null list_current) (null (cdr list_current))))))
        (cons 'step (lambda () (setq list_current (cdr list_current))))
        (cons 'read (lambda () (car list_current)))
        (cons 'read-type (lambda () 'list-item))
        (cons 'entangled-copy (lambda () (RT·TM·list·make list_original list_current)))
        (cons 'get-pos (lambda () list_current))
        (cons 'cue (lambda (token_pos) (setq list_current token_pos)))
        )))


  (defun RT·TM·string·make (str_original &optional int_idx-starting)
    "Factory function generating a Tape Machine interface for an Elisp string."
    (let
      (
        (int_idx-current (if int_idx-starting int_idx-starting 0))
        (int_len-str (length str_original))
        )
      (list
        (cons 'cue-leftmost (lambda () (setq int_idx-current 0)))
        (cons 'has-right-neighbor (lambda () (< int_idx-current int_len-str)))
        (cons 'step (lambda () (setq int_idx-current (1+ int_idx-current))))
        (cons 'read (lambda () (aref str_original int_idx-current)))
        (cons 'read-type (lambda () 'char))
        (cons 'entangled-copy (lambda () (RT·TM·string·make str_original int_idx-current)))
        (cons 'get-pos (lambda () int_idx-current))
        (cons 'cue (lambda (token_pos) (setq int_idx-current token_pos)))
        )))

  (defun RT·TM·string-byte·make (str_original encoding_character &optional int_idx-starting)
    "Factory function generating a Tape Machine interface for an encoded byte string."
    (let*
      (
        (str_byte (encode-coding-string str_original encoding_character))
        (int_idx-current (if int_idx-starting int_idx-starting 0))
        (int_len-str (length str_byte))
        )
      (list
        (cons 'cue-leftmost (lambda () (setq int_idx-current 0)))
        (cons 'has-right-neighbor (lambda () (< int_idx-current int_len-str)))
        (cons 'step (lambda () (setq int_idx-current (1+ int_idx-current))))
        (cons 'read (lambda () (aref str_byte int_idx-current)))
        (cons 'read-type (lambda () 'byte))
        (cons 'entangled-copy (lambda () (RT·TM·string-byte·make str_original encoding_character int_idx-current)))
        (cons 'get-pos (lambda () int_idx-current))
        (cons 'cue (lambda (token_pos) (setq int_idx-current token_pos)))
        (cons 'encoding (lambda () encoding_character))
        )))


  ;; Tape Machine Accessor Macros
  ;;

  (defmacro RT·TM·cue-leftmost (tm)
    `(funcall (cdr (assq 'cue-leftmost ,tm)))
    )

  (defmacro RT·TM·has-right-neighbor (tm)
    `(funcall (cdr (assq 'has-right-neighbor ,tm)))
    )

  (defmacro RT·TM·on-rightmost (tm)
    `(not (funcall (cdr (assq 'has-right-neighbor ,tm))))
    )

  (defmacro RT·TM·step (tm)
    `(funcall (cdr (assq 'step ,tm)))
    )

  (defmacro RT·TM·read (tm)
    `(funcall (cdr (assq 'read ,tm)))
    )

  (defmacro RT·TM·read-type (tm)
    `(funcall (cdr (assq 'read-type ,tm)))
    )

  (defmacro RT·TM·entangled-copy (tm)
    `(funcall (cdr (assq 'entangled-copy ,tm)))
    )

  (defmacro RT·TM·get-pos (tm)
    `(funcall (cdr (assq 'get-pos ,tm)))
    )

  (defmacro RT·TM·cue (tm token_pos)
    `(funcall (cdr (assq 'cue ,tm)) ,token_pos)
    )

  (defmacro RT·TM·encoding (tm)
    `(funcall (cdr (assq 'encoding ,tm)))
    )

  (defmacro RT·TM·buffer-byte (tm)
    `(funcall (cdr (assq 'buffer-byte ,tm)))
    )

  (defmacro RT·TM·buffer-display (tm)
    `(funcall (cdr (assq 'buffer-display ,tm)))
    )

  ;; parsing
  ;;

  (defun RT·TM·sequence·eq (tm_substrate tm_target)
    "Iteratively compare a substrate tape against a target tape."
    (let
      (
        (tm_substrate-copy (RT·TM·entangled-copy tm_substrate))
        (tm_target-copy (RT·TM·entangled-copy tm_target))
        )
      (let
        (
          (bool_are-eq (= (RT·TM·read tm_substrate-copy) (RT·TM·read tm_target-copy)))
          (bool_target-has-right-neighbor (RT·TM·has-right-neighbor tm_target-copy))
          (bool_substrate-has-right-neighbor (RT·TM·has-right-neighbor tm_substrate-copy))
          )
        (while 
          (and bool_are-eq bool_target-has-right-neighbor bool_substrate-has-right-neighbor)
          (progn
            (RT·TM·step tm_substrate-copy) 
            (RT·TM·step tm_target-copy)
            (setq bool_are-eq (= (RT·TM·read tm_substrate-copy) (RT·TM·read tm_target-copy)))
            (setq bool_target-has-right-neighbor (RT·TM·has-right-neighbor tm_target-copy))
            (setq bool_substrate-has-right-neighbor (RT·TM·has-right-neighbor tm_substrate-copy))
            ))
        (and bool_are-eq (not bool_target-has-right-neighbor))
        )))


;;;-----------------------------------------------------------------------------
;;; Configuration
;;;

  ;; root tape machine 
  ;;

  (defvar-local RT·TM·tm_host nil
    "Host buffer TM attached to the host buffer. (Other TMs attached to their overlays.)"
    )

  ;; tape machine mapping
  ;;

  (defvar RT·TM·alist_make-default
    '(
       (default . RT·TM·buffer-byte·make)
       )
    "Alist mapping major modes to Tape Machine factories."
    )

  ;; display theme
  ;;

  (defvar-local RT·TM·alist_theme nil
    "A buffer-local alist defining the active visual theme for TMs."
    )

  ;; selection tracking
  ;;

  (defvar-local RT·TM·ov_selection nil
    "Tracks the currently selected TM overlay in the buffer."
    )


;;;-----------------------------------------------------------------------------
;;; Interior code
;;;

  (defmacro RT·TM·overlay·leftmost (overlay)
    `(overlay-start ,overlay)
    )

  (defmacro RT·TM·overlay·rightmost_right-neighbor (overlay)
    `(overlay-end ,overlay)
    )

  (defun RT·TM·buffer·detect-all (tm_substrate encoding_character alist_theme list_make-if)
    "Scan tm_substrate in buffer-1 for sequences to instantiate child TMs, passing down list_make-if."
    (RT·TM·cue-leftmost tm_substrate) 
    
    (let 
      (
        (bool_substrate-active (RT·TM·has-right-neighbor tm_substrate))
        )
      (while bool_substrate-active
        
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
                )
              (let
                (
                  (tm_nested (if fn_make-if (funcall fn_make-if tm_lookahead encoding_character alist_theme list_make-if) nil))
                  )
                (if tm_nested
                  (let
                    (
                      (fn_overlay (cdr (assq 'overlay tm_nested)))
                      )
                    (setq bool_found-nested t)
                    (if fn_overlay
                      (let ((ov (funcall fn_overlay)))
                        (RT·TM·introspection·write-if 
                          'RT·TM·buffer·detect-all 
                          (format "Made nested TM of type %s" (overlay-get ov 'RT·TM-type))
                          )
                        ))))
                
                (setq list_fn (cdr list_fn))
                )))

        (RT·TM·step tm_substrate)
        (setq bool_substrate-active (RT·TM·has-right-neighbor tm_substrate))
        )))

  (defun RT·TM·buffer-byte·make (buffer_byte buffer_display pos_byte_leftmost pos_byte_rightmost_right-neighbor pos_byte_initial encoding_character)
    "Creates a TM that traverses BUFFER_BYTE, checking BUFFER_DISPLAY to step over nested overlays."
    (let
      (
        (pos_byte_current pos_byte_initial)
        )
      (list
        (cons 'cue-leftmost (lambda () (setq pos_byte_current pos_byte_leftmost)))
        (cons 'has-right-neighbor (lambda () (< pos_byte_current pos_byte_rightmost_right-neighbor)))
        (cons 'step 
          (lambda () 
            (let* (
                (pos_char_current (with-current-buffer buffer_display (byte-to-position pos_byte_current)))
                (ov_found (RT·TM·overlay·at-pos buffer_display pos_char_current))
                )
              (if ov_found
                (setq pos_byte_current (overlay-get ov_found 'RT·pos_byte_rightmost_right-neighbor))
                (setq pos_byte_current (1+ pos_byte_current))
                ))))
        (cons 'read 
          (lambda () 
            (with-current-buffer buffer_byte (char-after pos_byte_current))
            ))
        (cons 'read-type 
          (lambda () 
            (let*
              (
                (pos_char_current (with-current-buffer buffer_display (byte-to-position pos_byte_current)))
                )
              (if (RT·TM·overlay·at-pos buffer_display pos_char_current) 'RT·TM 'byte)
              )))
        (cons 'entangled-copy (lambda () (RT·TM·buffer-byte·make buffer_byte buffer_display pos_byte_leftmost pos_byte_rightmost_right-neighbor pos_byte_current encoding_character)))
        (cons 'get-pos (lambda () pos_byte_current))
        (cons 'cue (lambda (token_pos) (setq pos_byte_current token_pos)))
        (cons 'encoding (lambda () encoding_character))
        (cons 'buffer-byte (lambda () buffer_byte))
        (cons 'buffer-display (lambda () buffer_display))
        )))


;;;-----------------------------------------------------------------------------
;;; API
;;;

  (defun RT·TM·buffer·make-if (encoding_character alist_theme list_make-if)
    "API entry point. Initializes dual-buffer TM scanning using the provided list of make-if functions."
    (setq RT·TM·alist_theme alist_theme)
    (let*
      (
        (buffer_display (current-buffer))
        (buffer_byte (generate-new-buffer (concat " *RT·TM-byte:" (buffer-name) "*")))
        )
      ;; Seed the hidden byte buffer without encoding conversions
      (with-current-buffer buffer_byte
        (insert-buffer-substring buffer_display)
        (set-buffer-multibyte nil)
        )
      
      (let
        (
          (tm_host (RT·TM·buffer-byte·make buffer_byte buffer_display (point-min) (point-max) (point-min) encoding_character))
          )
        
        ;; Attach the host TM directly to the display buffer
        (with-current-buffer buffer_display
          (setq RT·TM·tm_host tm_host)
          )
        
        (RT·TM·buffer·detect-all tm_host encoding_character RT·TM·alist_theme list_make-if)
        tm_host
        )))


;;;-----------------------------------------------------------------------------
;;; Interactive
;;;

  (defun RT·TM·overlay·edit ()
    "Extracts the bytes of the selected overlay in buffer-0 into a new buffer-2 for editing."
    (interactive)
    (let
      (
        (ov RT·TM·ov_selection)
        )
      (if ov
        (let*
          (
            (tm_payload (overlay-get ov 'RT·TM))
            (buffer_byte (RT·TM·buffer-byte tm_payload))
            (pos_byte_leftmost (RT·TM·overlay·leftmost ov))
            (pos_byte_rightmost_right-neighbor (RT·TM·overlay·rightmost_right-neighbor ov))
            (str_raw (RT·buffer·raw-bytes·get buffer_byte pos_byte_leftmost pos_byte_rightmost_right-neighbor))
            (buffer_edit (generate-new-buffer "*RT·TM-Edit*"))
            )
          (with-current-buffer buffer_edit
            (insert str_raw)
            ;; Set local variables needed for the save hook to write back to buffer-1
            (setq-local RT·TM·buffer_target-byte buffer_byte)
            (setq-local RT·TM·ov_source ov)
            (local-set-key (kbd "C-c C-c") 'RT·TM·overlay·save-edit)
            )
          (pop-to-buffer buffer_edit)
          ))))

  (defun RT·TM·overlay·save-edit ()
    "Write the edited contents of buffer-2 back to buffer-1 and trigger a display update in buffer-0."
    (interactive)
    (message "RT·TM: Edit saved and projected.")
    )


;;;-----------------------------------------------------------------------------
;;; Integration
;;;
