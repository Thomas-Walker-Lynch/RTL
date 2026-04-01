;;; Emacs BreadBox
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


  ;; Introspection Utilities
  ;;

  (defun RT·BreadBox·introspection·write (source-buffer_name function_sym message_str)
    "Write MESSAGE_STR to the global introspection buffer, creating a new frame if needed."
    (let*
      (
        (introspection-buffer_name "*RT·BreadBox-Introspection*")
        (introspection_buffer (get-buffer-create introspection-buffer_name))
        (introspection_window (get-buffer-window introspection_buffer 0))
        )
      (if
        (null introspection_window)
        (let
          (
            (new_frame (make-frame '((name . "RT·BreadBox Introspection"))))
            )
          (set-window-buffer (frame-selected-window new_frame) introspection_buffer)
          ))
      (with-current-buffer introspection_buffer
        (goto-char (point-max))
        (insert (format "%s::%s::  %s\n" source-buffer_name function_sym message_str))
        )))

  (defmacro RT·BreadBox·introspection·write-if (function_sym message_str)
    "Log a message if FUNCTION_SYM is in the active introspection list."
    `(if
       (memq ,function_sym RT·BreadBox·introspection·symbol_list)
       (RT·BreadBox·introspection·write (buffer-name) ,function_sym ,message_str)
       ))


  ;; Tape Machine
  ;;

  (defun RT·TM·make-list-tape (original_list &optional starting_list)
    (let
      (
        (current_list (if starting_list starting_list original_list))
        )
      (list
        (cons 'cue-leftmost (lambda () (setq current_list original_list)))
        (cons 'on-rightmost (lambda () (or (null current_list) (null (cdr current_list)))))
        (cons 'step (lambda () (setq current_list (cdr current_list))))
        (cons 'read (lambda () (car current_list)))
        (cons 'entangled-copy (lambda () (RT·TM·make-list-tape original_list current_list)))
        (cons 'get-pos (lambda () current_list))
        ;; NEW: Jump directly to an opaque position token
        (cons 'cue (lambda (pos_token) (setq current_list pos_token)))
        )))

  (defun RT·TM·make-buffer-tape (leftmost_pos rightmost_pos initial_pos)
    (let
      (
        (current_pos initial_pos)
        )
      (list
        (cons 'cue-leftmost (lambda () (setq current_pos leftmost_pos)))
        (cons 'on-rightmost (lambda () (>= current_pos rightmost_pos)))
        (cons 'step (lambda () (setq current_pos (1+ current_pos))))
        (cons 'read (lambda () (char-after current_pos)))
        (cons 'entangled-copy (lambda () (RT·TM·make-buffer-tape leftmost_pos rightmost_pos current_pos)))
        (cons 'get-pos (lambda () current_pos))
        ;; NEW: Jump directly to an opaque position token
        (cons 'cue (lambda (pos_token) (setq current_pos pos_token)))
        )))

  (defun RT·TM·make-default-host-tape ()
    "Factory function for the default full-buffer tape machine."
    (RT·TM·make-buffer-tape (point-min) (1- (point-max)) (point-min))
    )

  ;; Tape Machine Accessor Macros
  ;;

  (defmacro RT·TM·cue-leftmost (tm)
    `(funcall (cdr (assq 'cue-leftmost ,tm)))
    )

  (defmacro RT·TM·on-rightmost (tm)
    `(funcall (cdr (assq 'on-rightmost ,tm)))
    )

  (defmacro RT·TM·step (tm)
    `(funcall (cdr (assq 'step ,tm)))
    )

  (defmacro RT·TM·read (tm)
    `(funcall (cdr (assq 'read ,tm)))
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


;;;-----------------------------------------------------------------------------
;;; Configuration
;;;

  ;; introspection
  ;;

  (defvar RT·BreadBox·introspection·symbol_list nil
    "List of function symbols enabled for introspection logging. Populated in the Integration section."
    )

  ;; tape machine mapping
  ;;

  (defvar RT·BreadBox·host-tm_alist
    '(
       (default . RT·TM·make-default-host-tape)
       )
    "Alist mapping major modes to Tape Machine factories."
    )


  ;; display theme
  ;;

  (defvar-local RT·BreadBox·theme_alist nil
    "A buffer-local alist defining the active visual theme for BreadBoxes.
     Format: ((state_sym . face_plist) ...)
     Example: ((background . (:background \"#333\")) (selected . (:box ...)))"
    )

  ;; selection tracking
  ;;

  (defvar-local RT·BreadBox·selection_ov nil
    "Tracks the currently selected BreadBox overlay in the buffer."
    )

  ;; overlay stack
  ;;

  (defvar-local RT·BreadBox·overlay·stack nil)
  (defvar-local RT·BreadBox·overlay·stack·selected nil)

  ;; display
  ;;

  (defvar RT·BreadBox·display·wrap-indent 2)

  ;; type registry
  ;;

  (defvar-local RT·BreadBox·type_alist nil
    "A buffer-local alist storing registered BreadBox types for the current buffer.
     Set by RT·BreadBox·buffer·scan. The key is the type symbol. The value is the definition list:
     '(type detect display edit has-right-neighbor right-neighbor nesting-allowed overlap-allowed)"
    )

;;;-----------------------------------------------------------------------------
;;; Interior code
;;;

  ;; overlay
  ;;

  (defun RT·BreadBox·theme·make-default ()
    "Factory function generating a dynamic theme alist relative to the host buffer's current colors."
    (RT·BreadBox·introspection·write-if 'RT·BreadBox·theme·make-default "Generating default dynamic theme.")
    (let*
      (
        (bg-raw_str (face-background 'default nil t))
        (fg-raw_str (face-foreground 'default nil t))
        (bg-color_str (if (or (null bg-raw_str) (string= bg-raw_str "unspecified-bg")) "#000000" bg-raw_str))
        (fg-color_str (if (or (null fg-raw_str) (string= fg-raw_str "unspecified-fg")) "#FFFFFF" fg-raw_str))
        (bg-hsl_list (apply 'color-rgb-to-hsl (color-name-to-rgb bg-color_str)))
        (fg-hsl_list (apply 'color-rgb-to-hsl (color-name-to-rgb fg-color_str)))
        (bg_h (nth 0 bg-hsl_list)) (bg_s (nth 1 bg-hsl_list)) (bg_l (nth 2 bg-hsl_list))
        (fg_h (nth 0 fg-hsl_list)) (fg_s (nth 1 fg-hsl_list)) (fg_l (nth 2 fg-hsl_list))
        (is-dark_bool (eq (frame-parameter nil 'background-mode) 'dark))
        (new-bg_l 0.0) (new-fg_l 0.0)
        )
      (if
        is-dark_bool
        (progn
          (setq new-bg_l (if (< (- bg_l 0.15) 0.0) (+ bg_l 0.15) (- bg_l 0.15)))
          (setq new-fg_l (if (> (+ fg_l 0.10) 1.0) 1.0 (+ fg_l 0.10)))
          (setq bg_s (min 1.0 (+ bg_s 0.1)))
          )
        (progn
          (setq new-bg_l (if (> (+ bg_l 0.15) 1.0) (- bg_l 0.15) (+ bg_l 0.15)))
          (setq new-fg_l (if (< (- fg_l 0.10) 0.0) 0.0 (- fg_l 0.10)))
          (setq bg_s (min 1.0 (+ bg_s 0.1)))
          ))
      (let*
        (
          (base_plist (list :background (apply 'color-rgb-to-hex (color-hsl-to-rgb bg_h bg_s new-bg_l))
                            :foreground (apply 'color-rgb-to-hex (color-hsl-to-rgb fg_h fg_s new-fg_l))))
          (selected_plist (append base_plist (list :box (list :line-width 1 :color (plist-get base_plist :foreground)))))
          (underline_plist (list :underline t))
          )
        ;; Return the theme alist
        (list
          (cons 'background base_plist)
          (cons 'selected selected_plist)
          (cons 'underline underline_plist)
          ))))  

  ;; TTCA topology mapping to Emacs boundaries
  ;;

  (defun RT·BreadBox·overlay·make (leftmost_pos rightmost_pos)
    (RT·BreadBox·introspection·write-if 'RT·BreadBox·overlay·make (format "Creating overlay from %d to %d" leftmost_pos rightmost_pos))
    (make-overlay leftmost_pos (1+ rightmost_pos))
    )

  (defun RT·BreadBox·overlay·wrap-on ()
    )

  (defun RT·BreadBox·overlay·wrap-off ()
    )

  (defmacro RT·BreadBox·overlay·leftmost (overlay)
    `(overlay-start ,overlay)
    )

  (defmacro RT·BreadBox·overlay·rightmost (overlay)
    `(1- (overlay-end ,overlay))
    )

  (defmacro RT·BreadBox·overlay·rightmost-right-neighbor (overlay)
    `(overlay-end ,overlay)
    )

  (defun RT·BreadBox·overlay·stack-is-empty ()
    (RT·BreadBox·stack·stack-is-empty RT·BreadBox·stack·overlay-stack)
    )

  ;; By contract, user gives non-null substrate_TM ad ov-type_to_fn-list_TM.
  ;; The substrate_TM is a sequence of characters/bytes that we are scanning over.
  ;; Each overlay type, ov-type, has its own detector function
  (defun RT·BreadBox·buffer·scan-tape (substrate_TM describer_dict theme_alist)
    "Scan substrate for sequences to put overlays over."

    (RT·TM·cue-leftmost substrate_TM) 
    (while 
      (progn
        
        (RT·TM·cue-leftmost ov-type_to_fn-list_TM)
        (while
          (let*
            ( ;; lookup the detect function to try
              (ov-type_and_fn_list (RT·TM·read ov-type_to_fn-list_TM))
              (ov-type (car ov-type_and_fn_list))
              (fn-list (cdr ov-type_and_fn_list))
              (detect (nth 1 fn-list))
              )
            

                (let
                  (
                    ;; Returns nil on failure, or '(ov payload_TM resume_pos) on success
                    (ov-characterization_list (funcall detect_lambda lookahead_TM))
                    )
                  (if ov-characterization_list
                    (let
                      (
                        (ov (nth 0 ov-characterization_list))
                        (payload_TM (nth 1 ov-characterization_list))
                        (resume_pos (nth 2 ov-characterization_list))
                        )
                      (setq found-nested_TM payload_TM)
                      (RT·BreadBox·introspection·write-if 
                        'RT·BreadBox·buffer·scan-tape 
                        (format "Detected type %s" (overlay-get ov 'RT·BreadBox·type))
                        )
                      
                      (if display_lambda
                        (funcall display_lambda ov nil theme_alist)
                        )
                      
                      ;; Recursive internal scan checked via overlay property
                      (if (overlay-get ov 'RT·BreadBox·can-be-nested)
                        (RT·BreadBox·buffer·scan-tape payload_TM ov-type_to_fn-list_alist theme_alist)
                        )
                      
                      ;; INSTANT JUMP: Bypass the payload entirely to avoid illegal character reads
                      (RT·TM·cue substrate_TM resume_pos)
                      ))
                  
                  ;; Center break logic for the ov-type_to_fn-list_TM iteration
                  (if (RT·TM·on-rightmost ov-type_to_fn-list_TM)
                    nil ;; Terminate inner loop
                    (progn
                      (RT·TM·step ov-type_to_fn-list_TM)
                      t   ;; Continue inner loop
                      ))
                  )))))))

          ;; Center break logic for the substrate_TM iteration
          (if (RT·TM·on-rightmost substrate_TM)
            nil ;; Terminate outer loop
            (progn
              (if (not found-nested_TM)
                (RT·TM·step substrate_TM)
                )
              t   ;; Continue outer loop
              ))
          ))


;;;-----------------------------------------------------------------------------
;;; API
;;;

  (defun RT·BreadBox·type·register (ov-type definition_list)
    "Register a new BreadBox type."
    (RT·BreadBox·introspection·write-if 'RT·BreadBox·type·register "Invoked.")
    (let
      (
        (existing_cons (assq ov-type RT·BreadBox·type_alist))
        )
      (if
        existing_cons
        (setcdr existing_cons definition_list)
        (push (cons ov-type definition_list) RT·BreadBox·type_alist)
        )))

  (defun RT·BreadBox·buffer·scan-host (ov-type_to_fn-list_alist theme_alist)
    "Entry point to scan the host document. Bootstraps the mode-specific tape machine."
    (RT·BreadBox·introspection·write-if 'RT·BreadBox·buffer·scan-host "Invoked.")
    (setq RT·BreadBox·type_alist ov-type_to_fn-list_alist)
    (setq RT·BreadBox·theme_alist theme_alist)
    (let
      (
        (factory_cons (assq major-mode RT·BreadBox·host-tm_alist))
        )
      (let
        (
          (tm_factory (if factory_cons (cdr factory_cons) (cdr (assq 'default RT·BreadBox·host-tm_alist))))
          )
        (let
          (
            (host_TM (funcall tm_factory))
            )
          (RT·BreadBox·buffer·scan-tape host_TM RT·BreadBox·type_alist RT·BreadBox·theme_alist)
          ))))

  (defun RT·BreadBox·buffer·insert-binary-payload-with-overlay (binary-data display-lambda)
    "Insert BINARY-DATA at point, cover it with an overlay, and call DISPLAY-LAMBDA."
    (RT·BreadBox·introspection·write-if 'RT·BreadBox·buffer·insert-binary-payload-with-overlay "Invoked.")
    (let
      (
        (leftmost_pos (point))
        )
      (insert binary-data)
      (let*
        (
          (rightmost_right-neighbor_pos (point))
          (payload-overlay (make-overlay leftmost_pos rightmost_right-neighbor_pos))
          )
        (funcall display-lambda payload-overlay binary-data)
        )))

  (defun RT·BreadBox·buffer·update-binary-backing (ov new-binary-data new-display-string)
    "Replace the binary data under OV with NEW-BINARY-DATA, and update display."
    (RT·BreadBox·introspection·write-if 'RT·BreadBox·buffer·update-binary-backing "Invoked.")
    (let
      (
        (leftmost_pos (overlay-start ov))
        (old-rightmost_right-neighbor_pos (overlay-end ov))
        )
      (save-excursion
        (goto-char leftmost_pos)
        (insert new-binary-data)
        (let
          (
            (new-rightmost_right-neighbor_pos (point))
            )
          (delete-region new-rightmost_right-neighbor_pos (+ new-rightmost_right-neighbor_pos (- old-rightmost_right-neighbor_pos leftmost_pos)))
          (move-overlay ov leftmost_pos new-rightmost_right-neighbor_pos)
          (overlay-put ov 'display new-display-string)
          ))))

  (defun RT·BreadBox·buffer·create-interactive-payload-overlay (leftmost_pos rightmost_right-neighbor_pos display-text label)
    "Create an overlay covering the topological payload bounds with a clickable DISPLAY-TEXT."
    (RT·BreadBox·introspection·write-if 'RT·BreadBox·buffer·create-interactive-payload-overlay "Invoked.")
    (let
      (
        (ov (make-overlay leftmost_pos rightmost_right-neighbor_pos))
        (map (make-sparse-keymap))
        )
      (define-key map [mouse-1] 'my-trigger-edit-function)
      (define-key map (kbd "RET") 'my-trigger-edit-function)
      (overlay-put ov 'display display-text)
      (overlay-put ov 'keymap map)
      (overlay-put ov 'pointer 'hand)
      ov
      ))

  (defun RT·BreadBox·select-cycle_cmd ()
    "Interactive command to cycle selection through BreadBox overlays at point."
    (interactive)
    (RT·BreadBox·introspection·write-if 'RT·BreadBox·select-cycle_cmd "Invoked.")
    (let*
      (
        (raw-ov_list (overlays-at (point)))
        (bb-ov_list nil)
        )
      ;; Filter for BreadBox overlays
      (dolist (ov raw-ov_list)
        (if
          (overlay-get ov 'RT·BreadBox·type)
          (push ov bb-ov_list)
          ))
      ;; Sort descending by length to guarantee largest is first
      (setq bb-ov_list
        (sort bb-ov_list
          (lambda (a b)
            (> (- (overlay-end a) (overlay-start a)) (- (overlay-end b) (overlay-start b)))
            )))
      (if
        bb-ov_list
        (let*
          (
            (current-tail_list (memq RT·BreadBox·selection_ov bb-ov_list))
            (current_idx (if current-tail_list (- (length bb-ov_list) (length current-tail_list)) nil))
            (next_ov nil)
            )
          ;; Unhighlight current selection if it exists
          (if
            RT·BreadBox·selection_ov
            (overlay-put RT·BreadBox·selection_ov 'face (cdr (assq 'background RT·BreadBox·theme_alist)))
            )
          ;; Determine next overlay in the cycle
          (if
            current_idx
            (let
              (
                (next_idx (1+ current_idx))
                )
              (if
                (>= next_idx (length bb-ov_list))
                (setq next_idx 0)
                )
              (setq next_ov (nth next_idx bb-ov_list))
              )
            ;; If none are currently selected, start with the largest (index 0)
            (setq next_ov (car bb-ov_list))
            )
          ;; Apply selected theme and save state
          (overlay-put next_ov 'face (cdr (assq 'selected RT·BreadBox·theme_alist)))
          (setq RT·BreadBox·selection_ov next_ov)
          (RT·BreadBox·introspection·write-if 'RT·BreadBox·select-cycle_cmd (format "Selected overlay type %s" (overlay-get next_ov 'RT·BreadBox·type)))
          )
        ;; No BreadBoxes at point
        (RT·BreadBox·introspection·write-if 'RT·BreadBox·select-cycle_cmd "No BreadBox overlays found at point.")
        )))

;;;-----------------------------------------------------------------------------
;;; Interactive
;;;

;;;-----------------------------------------------------------------------------
;;; Integration
;;;

  (setq RT·BreadBox·introspection·symbol_list
    '(
       RT·BreadBox·type·register
       RT·BreadBox·buffer·scan
       RT·BreadBox·buffer·insert-binary-payload-with-overlay
       RT·BreadBox·buffer·update-binary-backing
       RT·BreadBox·buffer·create-interactive-payload-overlay
       RT·BreadBox·overlay·make
       RT·BreadBox·example·detect-esc-hex
       RT·BreadBox·example·scan-and-highlight
       RT·BreadBox·example·highlight-demo
       ))

  (defun RT·BreadBox·keymap·make-default ()
    "Factory function generating the default BreadBox keymap."
    (RT·BreadBox·introspection·write-if 'RT·BreadBox·keymap·make-default "Invoked.")
    (let
      (
        (map (make-sparse-keymap))
        )
      (define-key map (kbd "m") 'RT·BreadBox·make_cmd)
      (define-key map (kbd "e") 'RT·BreadBox·edit_cmd)
      (define-key map (kbd "s") 'RT·BreadBox·select-cycle_cmd)
      (define-key map (kbd "a") 'RT·BreadBox·edit-abort_cmd)
      (define-key map (kbd "x") 'RT·BreadBox·edit-exit_cmd)
      ;; [o]ptions
      (define-key map (kbd "o i 1") 'RT·BreadBox·overlay·wrap-on_cmd)
      (define-key map (kbd "o i 0") 'RT·BreadBox·overlay·wrap-off_cmd)
      map
      ))


;;;-----------------------------------------------------------------------------
;;; example-highlight
;;;
;;;   overlay highlighting demonstrator
;;;

  (defun RT·BreadBox·example·highlight-demo ()
    "Interactive command demonstrating various BreadBox highlight styles."
    (interactive)
    (RT·BreadBox·introspection·write-if 'RT·BreadBox·example·highlight-demo "Invoked.")
    (let*
      (
        (demo_buffer (get-buffer-create "*RT·BreadBox-Demo*"))
        (active_theme (RT·BreadBox·theme·make-default))
        (bg_plist (cdr (assq 'background active_theme)))
        (sel_plist (cdr (assq 'selected active_theme)))
        (ul_plist (cdr (assq 'underline active_theme)))
        )
      (with-current-buffer demo_buffer
        (erase-buffer)
        (insert "BreadBox Highlight Demonstrations\n\n")

        (let
          (
            (leftmost_pos (point))
            )
          (insert "This text uses the shade highlight.")
          (let*
            (
              (rightmost_right-neighbor_pos (point))
              (ov (RT·BreadBox·overlay·make leftmost_pos (1- rightmost_right-neighbor_pos)))
              )
            (overlay-put ov 'face bg_plist)
            ))
        (insert "\n\n")

        (let
          (
            (leftmost_pos (point))
            )
          (insert "This text uses the box highlight (selection style).")
          (let*
            (
              (rightmost_right-neighbor_pos (point))
              (ov (RT·BreadBox·overlay·make leftmost_pos (1- rightmost_right-neighbor_pos)))
              )
            (overlay-put ov 'face sel_plist)
            ))
        (insert "\n\n")

        (let
          (
            (leftmost_pos (point))
            )
          (insert "This text uses the underline highlight.")
          (let*
            (
              (rightmost_right-neighbor_pos (point))
              (ov (RT·BreadBox·overlay·make leftmost_pos (1- rightmost_right-neighbor_pos)))
              )
            (overlay-put ov 'face ul_plist)
            ))
        (insert "\n")
        )
      (switch-to-buffer demo_buffer)
      ))


 
;;;-----------------------------------------------------------------------------
;;; example-escape-literal 
;;;
;;;   Balanced ESC characters delimited the bread box literal within a text buffer.
;;;
;;;   The bread box literal displays as hex, and opens for edit in another buffer in hex mode for editing.
;;;
;;;   bread box literals can be nested.
;;;
;;;   When nested, the outer literal opens for edit as though inner literals are data.
;;;
;;;   When nested, initially the outermost literal is opened, then successive select rotates through the literals.
;;;

  ;; externally defined lambdas that get passed into BreadBox interface functions
  ;;

(defun RT·BreadBox·example-escape-literal·detect-esc-hex (lookahead_TM)
    "Detector operating over a Tape Machine. 
     Returns '(overlay payload_TM resume_pos) on success, or nil on failure."
    (let
      (
        (start_val (RT·TM·read lookahead_TM))
        )
      (if
        (eq start_val ?\e)
        (let
          (
            (leftmost_pos (RT·TM·get-pos lookahead_TM))
            )
          (RT·BreadBox·introspection·write-if 'RT·BreadBox·example-escape-literal·detect-esc-hex "Leftmost ESC found.")
          (RT·TM·step lookahead_TM)
          
          (let
            (
              (found-end_bool
                (catch 'found
                  (while (progn
                    (if (RT·TM·on-rightmost lookahead_TM)
                      nil
                      (let ((next_val (RT·TM·read lookahead_TM)))
                        (if (eq next_val ?\e)
                          (throw 'found t)
                          (progn (RT·TM·step lookahead_TM) t)
                          )))))
                  nil
                  ))
              )
            (if found-end_bool
              (let
                (
                  (rightmost-right-neighbor_pos (1+ (RT·TM·get-pos lookahead_TM)))
                  )
                (RT·BreadBox·introspection·write-if 'RT·BreadBox·example-escape-literal·detect-esc-hex "Rightmost ESC found.")
                
                ;; Advance the lookahead TM exactly one cell past the BreadBox to capture the resume token
                (if (not (RT·TM·on-rightmost lookahead_TM))
                  (RT·TM·step lookahead_TM)
                  )
                  
                (let*
                  (
                    (resume_pos (RT·TM·get-pos lookahead_TM))
                    (rightmost_pos (1- rightmost-right-neighbor_pos))
                    (ov (RT·BreadBox·overlay·make leftmost_pos rightmost_pos))
                    (payload_TM (RT·TM·make-buffer-tape (1+ leftmost_pos) (1- rightmost_pos) (1+ leftmost_pos)))
                    )
                  (overlay-put ov 'RT·BreadBox·type 'esc-hex)
                  (overlay-put ov 'RT·BreadBox·can-be-nested t)
                  
                  (list ov payload_TM resume_pos)
                  ))
              nil
              )))
        nil
        )))

  (defun RT·BreadBox·example-escape-literal·display (ov binary-data theme_alist)
    "Display lambda called by the scanner or inserter."
    (RT·BreadBox·introspection·write-if 'RT·BreadBox·example-escape-literal·display "Invoked.")
    (let*
      (
        (payload_str (buffer-substring-no-properties (1+ (overlay-start ov)) (1- (overlay-end ov))))
        (hex_str (mapconcat (lambda (c) (format "%02x" c)) payload_str " "))
        (bg_plist (cdr (assq 'background theme_alist)))
        )
      (overlay-put ov 'face bg_plist)
      (overlay-put ov 'display hex_str)
      ))

  ;; Interactive interface
  ;;

  (defun RT·BreadBox·example-escape-literal·scan-and-highlight ()
    "Interactive command to scan the buffer and highlight all ESC-delimited BreadBoxes."
    (interactive)
    (RT·BreadBox·introspection·write-if 'RT·BreadBox·example-escape-literal·scan-and-highlight "Invoked.")
    (let
      (
        (active_theme (RT·BreadBox·theme·make-default))
        (example_type_alist
          (list
            (cons 'esc-hex
              (list
                'esc-hex
                'RT·BreadBox·example-escape-literal·detect-esc-hex
                'RT·BreadBox·example-escape-literal·display  
                nil
                nil
                nil
                nil
                nil
                ))))
        )
      ;; Call the host scan entry point
      (RT·BreadBox·buffer·scan-host example_type_alist active_theme)
      ))
