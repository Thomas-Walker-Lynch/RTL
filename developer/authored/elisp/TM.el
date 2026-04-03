;;; Emacs TM (Tape Machine)
;;;

;;;------------------------------------------------------------------------------
;;; Utilities
;;;

  ;; Generic Stack Interface
  ;;

  (defmacro RT·stack·push (stack_sym item)
    `(push ,item ,stack_sym)
    )

  (defmacro RT·stack·pop (stack_sym)
    `(pop ,stack_sym)
    )

  (defun RT·stack·top (stack_list)
    (car stack_list)
    )

  (defmacro RT·stack·is-empty (stack_sym)
    `(null ,stack_sym)
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
        (existing_cons (assq key (cdr dict)))
        )
      (if
        existing_cons
        (setcdr existing_cons value)
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

  (defun RT·buffer·byte-at-pos (byte_pos encoding)
    "Extract the exact byte at absolute BYTE_POS, simulated under ENCODING."
    (let*
      (
        (char_pos (byte-to-position byte_pos))
        (char_start_byte (position-bytes char_pos))
        (byte_offset (- byte_pos char_start_byte))
        (char_str (string (char-after char_pos)))
        (bytes_str (encode-coding-string char_str encoding))
        )
      (aref bytes_str byte_offset)
      ))

  (defun RT·buffer·raw-bytes·get (start end)
    "Extract the exact file bytes for the text between START and END."
    (let ((text (buffer-substring-no-properties start end)))
      ;; buffer-file-coding-system holds the encoding used for the current file
      (encode-coding-string text buffer-file-coding-system)))

  (defun RT·TM·overlay·at-pos (pos)
    "Return the first overlay at POS that contains an RT·TM property."
    (let
      (
        (ov_list (overlays-at pos))
        (found_ov nil)
        )
      (while (and ov_list (not found_ov))
        (let ((ov (car ov_list)))
          (if (overlay-get ov 'RT·TM)
            (setq found_ov ov)
            (setq ov_list (cdr ov_list))
            )))
      found_ov
      ))

  ;; Tape Machine
  ;;
  ;;   Tape machines can be nested, so the tape machine that we hold a reference to is called tape machine "V0".  A tape nested in V0 is said to be "V1"

  (defun RT·TM·list·make (original_list &optional starting_list)
    (let
      (
        (current_list (if starting_list starting_list original_list))
        )
      (list
        (cons 'cue-leftmost (lambda () (setq current_list original_list)))
        (cons 'has-right-neighbor (lambda () (not (or (null current_list) (null (cdr current_list))))))
        (cons 'step (lambda () (setq current_list (cdr current_list))))
        (cons 'read (lambda () (car current_list)))
        (cons 'read-type (lambda () 'list-item))
        (cons 'entangled-copy (lambda () (RT·TM·list·make original_list current_list)))
        (cons 'get-pos (lambda () current_list))
        (cons 'cue (lambda (pos_token) (setq current_list pos_token)))
        )))

  (defun RT·TM·buffer·make (leftmost_pos rightmost_pos initial_pos)
    (let
      (
        (current_pos initial_pos)
        )
      (list
        (cons 'cue-leftmost (lambda () (setq current_pos leftmost_pos)))
        (cons 'has-right-neighbor (lambda () (< current_pos rightmost_pos)))
        (cons 'step 
          (lambda () 
            (let ((ov (RT·TM·overlay·at-pos current_pos)))
              (if ov
                (setq current_pos (overlay-end ov))
                (setq current_pos (1+ current_pos))
                ))))
        (cons 'read 
          (lambda () 
            (let ((ov (RT·TM·overlay·at-pos current_pos)))
              (if ov (overlay-get ov 'RT·TM) (char-after current_pos))
              )))
        (cons 'read-type 
          (lambda () 
            (if (RT·TM·overlay·at-pos current_pos) 'RT·TM 'char)
            ))
        (cons 'entangled-copy (lambda () (RT·TM·buffer·make leftmost_pos rightmost_pos current_pos)))
        (cons 'get-pos (lambda () current_pos))
        (cons 'cue (lambda (pos_token) (setq current_pos pos_token)))
        )))

  (defun RT·TM·buffer-byte·make (leftmost_byte rightmost_byte initial_byte encoding)
    (let
      (
        (current_byte initial_byte)
        )
      (list
        (cons 'cue-leftmost (lambda () (setq current_byte leftmost_byte)))
        (cons 'has-right-neighbor (lambda () (< current_byte rightmost_byte)))
        (cons 'step 
          (lambda () 
            (let ((ov (RT·TM·overlay·at-pos (byte-to-position current_byte))))
              (if ov
                (setq current_byte (position-bytes (overlay-end ov)))
                (setq current_byte (1+ current_byte))
                ))))
        (cons 'read (lambda () (RT·buffer·byte-at-pos current_byte encoding)))
        (cons 'read-type (lambda () (if (RT·TM·overlay·at-pos (byte-to-position current_byte)) 'RT·TM 'byte)))
        (cons 'entangled-copy (lambda () (RT·TM·buffer-byte·make leftmost_byte rightmost_byte current_byte encoding)))
        (cons 'get-pos (lambda () current_byte))
        (cons 'cue (lambda (pos_token) (setq current_byte pos_token)))
        (cons 'encoding (lambda () encoding))
        )))

  (defun RT·TM·string·make (original_string &optional starting_idx)
    "Factory function generating a Tape Machine interface for an Elisp string."
    (let
      (
        (current_idx (if starting_idx starting_idx 0))
        (str_len (length original_string))
        )
      (list
        (cons 'cue-leftmost (lambda () (setq current_idx 0)))
        (cons 'has-right-neighbor (lambda () (< current_idx str_len)))
        (cons 'step (lambda () (setq current_idx (1+ current_idx))))
        (cons 'read (lambda () (aref original_string current_idx)))
        (cons 'read-type (lambda () 'char))
        (cons 'entangled-copy (lambda () (RT·TM·string·make original_string current_idx)))
        (cons 'get-pos (lambda () current_idx))
        (cons 'cue (lambda (pos_token) (setq current_idx pos_token)))
        )))

  (defun RT·TM·string-byte·make (original_string encoding &optional starting_idx)
    (let*
      (
        (byte_str (encode-coding-string original_string encoding))
        (current_idx (if starting_idx starting_idx 0))
        (str_len (length byte_str))
        )
      (list
        (cons 'cue-leftmost (lambda () (setq current_idx 0)))
        (cons 'has-right-neighbor (lambda () (< current_idx str_len)))
        (cons 'step (lambda () (setq current_idx (1+ current_idx))))
        (cons 'read (lambda () (aref byte_str current_idx)))
        (cons 'read-type (lambda () 'byte))
        (cons 'entangled-copy (lambda () (RT·TM·string-byte·make original_string encoding current_idx)))
        (cons 'get-pos (lambda () current_idx))
        (cons 'cue (lambda (pos_token) (setq current_idx pos_token)))
        (cons 'encoding (lambda () encoding))
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

  (defmacro RT·TM·cue (tm pos_token)
    `(funcall (cdr (assq 'cue ,tm)) ,pos_token)
    )

  (defmacro RT·TM·encoding (tm)
    `(funcall (cdr (assq 'encoding ,tm)))
    )

  ;; Introspection Utilities
  ;;

  (defun RT·TM·introspection·write (source-buffer_name function_sym message_str)
    "Write MESSAGE_STR to the global introspection buffer, creating a new frame if needed."
    (let*
      (
        (introspection-buffer_name "*RT·TM-Introspection*")
        (introspection_buffer (get-buffer-create introspection-buffer_name))
        (introspection_window (get-buffer-window introspection_buffer 0))
        )
      (if
        (null introspection_window)
        (let
          (
            (new_frame (make-frame '((name . "RT·TM Introspection"))))
            )
          (set-window-buffer (frame-selected-window new_frame) introspection_buffer)
          ))
      (with-current-buffer introspection_buffer
        (goto-char (point-max))
        (insert (format "%s::%s::  %s\n" source-buffer_name function_sym message_str))
        )))

  (defmacro RT·TM·introspection·write-if (function_sym message_str)
    "Log a message if FUNCTION_SYM is in the active introspection list."
    `(if
       (memq ,function_sym RT·TM·introspection·symbol_list)
       (RT·TM·introspection·write (buffer-name) ,function_sym ,message_str)
       ))

  ;; parsing
  ;;

  (defun RT·TM·sequence·eq (TM_substrate TM_target)
    "Iteratively compare a substrate tape against a target tape.
     Works universally for Character TMs or Byte TMs."
    (let
      (
        (TM-substrate-copy (RT·TM·entangled-copy TM_substrate))
        (TM-target-copy (RT·TM·entangled-copy TM_target))
        )
      (let
        (
          (are-eq (= (RT·TM·read TM-substrate-copy) (RT·TM·read TM-target-copy)))
          (target-has-right-neighbor (RT·TM·has-right-neighbor TM-target-copy))
          (substrate-has-right-neighbor (RT·TM·has-right-neighbor TM-substrate-copy))
          )
        (while 
          (and are-eq target-has-right-neighbor substrate-has-right-neighbor)
          (progn
            (RT·TM·step TM-substrate-copy) 
            (RT·TM·step TM-target-copy)
            (setq are-eq (= (RT·TM·read TM-substrate-copy) (RT·TM·read TM-target-copy)))
            (setq target-has-right-neighbor (RT·TM·has-right-neighbor TM-target-copy))
            (setq substrate-has-right-neighbor (RT·TM·has-right-neighbor TM-substrate-copy))
            ))
        (and are-eq (not target-has-right-neighbor))
        )))


;;;-----------------------------------------------------------------------------
;;; Configuration
;;;

  ;; introspection
  ;;

  (defvar RT·TM·introspection·symbol_list nil
    "List of function symbols enabled for introspection logging. Populated in the Integration section."
    )

  ;; tape machine mapping
  ;;

  (defvar RT·TM·buffer-default·make
    '(
       (default . RT·TM·buffer·make)
       )
    "Alist mapping major modes to Tape Machine factories."
    )

  ;; display theme
  ;;

  (defvar-local RT·TM·theme_alist nil
    "A buffer-local alist defining the active visual theme for TMs."
    )

  ;; selection tracking
  ;;

  (defvar-local RT·TM·selection_ov nil
    "Tracks the currently selected TM overlay in the buffer."
    )

  ;; type registry
  ;;

  (defvar-local RT·TM·dict_TM-type_to_TM-interface nil
    "A dictionary mapping a TM-type to its interface closure list."
    )


;;;-----------------------------------------------------------------------------
;;; Interior code
;;;

  ;; TM interface entry accessors
  ;;
  ;; An entry is a cons cell:
  ;; (tm-type . (type make detect display edit has-right-neighbor right-neighbor can-be-nested overlap-allowed))
  ;;

  (defmacro RT·TM·interface·tm-type (entry)
    `(car ,entry)
    )

  (defmacro RT·TM·interface·make (entry)
    `(nth 1 (cdr ,entry))
    )

  (defmacro RT·TM·interface·detect (entry)
    `(nth 2 (cdr ,entry))
    )

  (defmacro RT·TM·interface·display (entry)
    `(nth 3 (cdr ,entry))
    )

  (defmacro RT·TM·interface·edit (entry)
    `(nth 4 (cdr ,entry))
    )

  (defmacro RT·TM·interface·has-right-neighbor (entry)
    `(nth 5 (cdr ,entry))
    )

  (defmacro RT·TM·interface·right-neighbor (entry)
    `(nth 6 (cdr ,entry))
    )

  (defmacro RT·TM·interface·can-be-nested (entry)
    `(nth 7 (cdr ,entry))
    )

  (defmacro RT·TM·interface·overlap-allowed (entry)
    `(nth 8 (cdr ,entry))
    )

  ;; overlay
  ;;

  (defun RT·TM·overlay·make (leftmost_pos rightmost_pos)
    (make-overlay leftmost_pos (1+ rightmost_pos))
    )

  (defmacro RT·TM·overlay·leftmost (overlay)
    `(overlay-start ,overlay)
    )

  (defmacro RT·TM·overlay·rightmost (overlay)
    `(1- (overlay-end ,overlay))
    )


  (defun RT·TM·buffer·detect-all (substrate_TM character-encoding theme_alist)
    "Scan substrate for sequences to instantiate child TMs over using the first-rest pattern."
    (RT·TM·cue-leftmost substrate_TM) 
    
    (let 
      (
        (substrate-active (RT·TM·has-right-neighbor substrate_TM))
        )
      (while substrate-active
        
        (let*
          (
            (dict_TM (RT·TM·dict·make RT·TM·dict_TM-type_to_TM-interface))
            (dict-active (RT·TM·has-right-neighbor dict_TM))
            (found-nested nil)
            )
          (while (and dict-active (not found-nested))
            (let*
              (
                (entry (RT·TM·read dict_TM))
                (detect_lambda (RT·TM·interface·detect entry))
                (display_lambda (RT·TM·interface·display entry))
                (lookahead_TM (RT·TM·entangled-copy substrate_TM))
                )
              (let
                (
                  (nested_TM (funcall detect_lambda lookahead_TM character-encoding))
                  )
                (if nested_TM
                  (let
                    (
                      (ov (overlay-get nested_TM 'overlay))
                      )
                    (setq found-nested t)
                    (RT·TM·introspection·write-if 
                      'RT·TM·buffer·detect-all 
                      (format "Detected type %s" (overlay-get ov 'RT·TM-type))
                      )
                    
                    (if display_lambda
                      (funcall display_lambda ov nil theme_alist)
                      )
                    
                    (if (RT·TM·interface·can-be-nested entry)
                      (RT·TM·buffer·detect-all nested_TM character-encoding theme_alist)
                      )
                    ))))
            
            (RT·TM·step dict_TM)
            (setq dict-active (RT·TM·has-right-neighbor dict_TM))
            ))

        (RT·TM·step substrate_TM)
        (setq substrate-active (RT·TM·has-right-neighbor substrate_TM))
        )))

    (defun RT·TM·display·project-overlay (buffer-1 buffer-0 byte_start byte_end hex_string)
      "Projects a detected byte range from BUFFER-1 as a display overlay in BUFFER-0."
      (with-current-buffer buffer-0
        (let*
          (
            (char_start (byte-to-position byte_start))
            (char_end (byte-to-position byte_end))
            (display_ov (make-overlay char_start char_end))
            )
          (overlay-put display_ov 'display hex_string)
          (overlay-put display_ov 'RT·TM-type 'byte-quote)
          ;; Bind the TM instance to the display overlay for future interaction
          display_ov
          )))

;;;-----------------------------------------------------------------------------
;;; API
;;;

  (defun RT·TM·type·register (tm-type interface_list)
    "Register the canonical definition of a TM type."
    (if
      (null RT·TM·dict_TM-type_to_TM-interface)
      (setq RT·TM·dict_TM-type_to_TM-interface (RT·dict·make))
      )
    (RT·dict·write RT·TM·dict_TM-type_to_TM-interface tm-type interface_list)
    )

  (defun RT·TM·make (tm-type character-encoding &rest args)
    "Wrapper to dynamically instantiate a TM of tm-type."
    (let*
      (
        (entry (RT·dict·read RT·TM·dict_TM-type_to_TM-interface tm-type))
        (make-fn (RT·TM·interface·make entry))
        )
      (if make-fn
        (apply make-fn character-encoding args)
        (progn
          (message "RT·TM: make failed, type %s unsupported or missing." tm-type)
          nil
          ))))

  (defun RT·TM·buffer·setup-and-discover-all (character-encoding theme_alist)
    "Entry point to instantiate the root buffer TM and scan the host document."
    (setq RT·TM·theme_alist theme_alist)
    (let
      (
        (host_TM (RT·TM·buffer·make (point-min) (1- (point-max)) (point-min)))
        )
      (RT·TM·buffer·detect-all host_TM character-encoding RT·TM·theme_alist)
      ))

;;;-----------------------------------------------------------------------------
;;; Integration
;;;

  (setq RT·TM·introspection·symbol_list
    '(
       RT·TM·type·register
       RT·TM·buffer·detect-all
       RT·TM·example-escape-literal·detect-quotes
       ))

;;;-----------------------------------------------------------------------------
;;; example-escape-literal 
;;;

  (defun RT·TM·example-escape-literal·detect-quotes (TM_substrate character-encoding)
    "Detector matching the d“ prefix and ”b suffix. 
     Returns the fully bound byte payload TM on success, or nil on failure."
    (let
      (
        (prefix_TM (RT·TM·string·make "d“"))
        (suffix_TM (RT·TM·string-byte·make "”b" character-encoding))
        )
      (if
        (RT·TM·sequence·eq TM_substrate prefix_TM)
        (let*
          (
            (ov-leftmost_char_pos (RT·TM·get-pos TM_substrate))
            (lookahead_char_TM (RT·TM·entangled-copy TM_substrate))
            (prefix_len_chars (length "d“"))
            )
          
          (let ((skips prefix_len_chars))
            (while (> skips 0)
              (RT·TM·step lookahead_char_TM)
              (setq skips (1- skips))
              ))
          
          (let*
            (
              (payload-start_char_pos (RT·TM·get-pos lookahead_char_TM))
              (payload-start_byte_pos (position-bytes payload-start_char_pos))
              (lookahead_byte_TM (RT·TM·buffer-byte·make payload-start_byte_pos (position-bytes (point-max)) payload-start_byte_pos character-encoding))
              )
            
            (let
              (
                (found-end_bool
                  (catch 'found
                    (while
                      (if (not (RT·TM·has-right-neighbor lookahead_byte_TM))
                        nil
                        (if (RT·TM·sequence·eq lookahead_byte_TM suffix_TM)
                          (throw 'found t)
                          (progn
                            (RT·TM·step lookahead_byte_TM)
                            t
                            ))))
                    nil
                    ))
                )
              (if found-end_bool
                (let*
                  (
                    (payload-rightmost_byte_pos (1- (RT·TM·get-pos lookahead_byte_TM)))
                    (resume_byte_pos (1+ payload-rightmost_byte_pos))
                    (resume_char_pos (byte-to-position resume_byte_pos))
                    (ov-rightmost_char_pos (1- resume_char_pos))
                    
                    (ov (RT·TM·overlay·make ov-leftmost_char_pos ov-rightmost_char_pos))
                    (payload_TM (RT·TM·buffer-byte·make payload-start_byte_pos payload-rightmost_byte_pos payload-start_byte_pos character-encoding))
                    )
                  
                  (overlay-put ov 'RT·TM-type 'byte-quote)
                  (overlay-put ov 'RT·TM payload_TM)
                  
                  ;; Manually push the overlay into the payload TM dictionary for reference
                  (setcdr (last payload_TM) (list (cons 'overlay (lambda () ov))))
                  
                  payload_TM
                  )
                nil
                ))))
        nil
        )))
