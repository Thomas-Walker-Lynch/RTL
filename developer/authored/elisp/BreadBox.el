;;; Emacs BreadBox implementation
;;;
;;;   The purpose of a BreadBox is to drop literal data, no matter what its contents, no matter what it represents, perhaps very short, perhaps very long, into an Emacs buffer, independent of the mode the buffer is in.  This can be used to create language literal strings without embedded escape sequences, or even to include raw binary such as media objects.
;;;
;;;   When used with the extent-literal, Conforming compilers or interpreters will then facilitate using the contents of the BreadBox as a variable initializer. 
;;;
;;;   A BreadBox instance is a sequence of bytes. This library does not directly examine the contents of a BreadBox, but rather, it is given externally defined lambdas that tell the library about the BreadBox. BreadBoxes are typed, and each type has a detector function associated with it:
;;;
;;;     '(type 
;;;        detect 
;;;        display 
;;;        edit 
;;;        has-right-neighbor 
;;;        right-neighbor 
;;;        nesting-allowed overlap-allowed
;;;        )
;;;
;;;   `type` is a symbol. `detect` is passed a buffer position, and returns nil if the BreadBox is not detected at that position. Otherwise it returns an Emac's overlay that covers the BreadBox. The returned overlay has attached to it the symbol for its type.
;;;   Before using the type to lookup the lambda, the overlay is first examined for an override, thus dispatch is done by a helper function.
;;;
;;;   A buffer must be scanned as first operation to detect all the BreadBoxes, and as BreadBoxes can be nested, the scan includes both the buffer in its mode, and internally through the BreadBox in its mode (using the `right-neighbor` lambda).
;;;
;;;   `edit` is externally defined. One possibility is that it opens another panel for editing the BreadBox contents in another panel. Some BreadBoxes can be nested, so if a user opens a new BreadBox or edits a contained one, the current edit is pushed on to the buffer's RT·BreadBox·overlay·stack. Then editing of the outer BreadBox can continue after the inner BreadBox is finished.
;;;
;;;   The utility `panel·open-temporary` provided with the library re-uses an already open panel on the right, or if the main buffer is on a panel on the right, it reuses the one on the left.  If the window is too narrow for side by side panels, it opens a panel below.  If the window is too small, it opens a new frame. If it opens a new panel or frame, it is temporary. It reuses a panel, then the original contents are restored when the edit is finished.
;;;
;;;   Display wrap is an overlay attribute. There are multiple options. A utility function uses this algorithm: when an indention is needed, it is taken as the indention of the line that the BreadBox appears on, plus the amount in the indent configuration variable.
;;;
;;;   Selecting an overlay can return a list of overlays at the given position found by traversing the internal overlay tree. The list returned by Emacs is reversed, and the initial selection is then the largest overlay. Selecting again cycles through the overlays in the list. The highlight function used by overlay selection is a library configuration parameters.
;;;
;;;   Our `edit` function is actually 'edit selected',  hence an overlay must be selected before its contents are edited.
;;;
;;;   The example section at the bottom uses an ASCII ESC character on the left to start a binary field, allows hex entry/display of the field, and ends with an ESC field. Yes, this example is flawed, a user could edit the binary and insert and ESC. The ESC characters are not shown in the display, as they are not part of the payload.
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
    "Factory function generating a Tape Machine interface for a Lisp list."
    (let
      (
        (current_list (if starting_list starting_list original_list))
        )
      (list
        (cons 'cue-leftmost 
          (lambda () 
            (setq current_list original_list)))
            
        (cons 'on-rightmost 
          (lambda () 
            ;; A list is on its rightmost cell if it has one or zero items left
            (or (null current_list) (null (cdr current_list)))))
            
        (cons 'step 
          (lambda () 
            ;; Contract: Caller verified not on-rightmost()
            (setq current_list (cdr current_list))))
            
        (cons 'read 
          (lambda () 
            (car current_list)))
            
        (cons 'entangled-copy 
          (lambda ()
            (RT·TM·make-list-tape original_list current_list)))
            
        (cons 'get-pos 
          (lambda () 
            current_list))
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

  (defun RT·BreadBox·overlay·highlight-none ()
    nil
    )

  (defun RT·BreadBox·overlay·highlight-default ()
    'region
    )

  (defun RT·BreadBox·overlay·highlight-shades ()
    (let*
      (
        (bg-raw_str (face-background 'default nil t))
        (fg-raw_str (face-foreground 'default nil t))
        (bg-color_str
          (if
            (or (null bg-raw_str) (string= bg-raw_str "unspecified-bg"))
            "#000000"
            bg-raw_str
            ))
        (fg-color_str
          (if
            (or (null fg-raw_str) (string= fg-raw_str "unspecified-fg"))
            "#FFFFFF"
            fg-raw_str
            ))
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

  (defun RT·literal·apply-highlight (overlay)
    (let
      (
        (face_prop (funcall RT·literal·highlight_func))
        )
      (if
        face_prop
        (overlay-put overlay 'face face_prop)
        (overlay-put overlay 'face nil)
        )))

  (defun RT·literal·apply-selection (overlay)
    (let*
      (
        (base-face_list (funcall RT·literal·highlight_func))
        (fg-color_str (plist-get base-face_list :foreground))
        (selected-face_list (append base-face_list (list :box (list :line-width 1 :color fg-color_str))))
        )
      (if
        base-face_list
        (overlay-put overlay 'face selected-face_list)
        )))


;;;-----------------------------------------------------------------------------
;;; API
;;;

  (defun RT·BreadBox·type·register (type_sym definition_list)
    "Register a new BreadBox type."
    (RT·BreadBox·introspection·write-if 'RT·BreadBox·type·register "Invoked.")
    (let
      (
        (existing_cons (assq type_sym RT·BreadBox·type_alist))
        )
      (if
        existing_cons
        (setcdr existing_cons definition_list)
        (push (cons type_sym definition_list) RT·BreadBox·type_alist)
        )))

  (defun RT·BreadBox·buffer·scan-host (type_alist theme_alist)
    "Entry point to scan the host document. Bootstraps the mode-specific tape machine."
    (RT·BreadBox·introspection·write-if 'RT·BreadBox·buffer·scan-host "Invoked.")
    (setq RT·BreadBox·type_alist type_alist)
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

  (defun RT·BreadBox·buffer·scan-tape (target_TM type_alist theme_alist)
    "Universal scanner executing over a generalized Tape Machine."
    (RT·TM·cue-leftmost target_TM)
    (while
      (let
        (
          (found-nested_TM nil)
          (type_TM (RT·TM·make-list-tape type_alist))
          )
        (if type_alist (RT·TM·cue-leftmost type_TM))
          
        ;; Detect phase using the type_TM
        (while
          (if (or (null type_alist) found-nested_TM)
            nil ;; Break type loop if we have no types or already found a match
            (let
              (
                (registry_cons (RT·TM·read type_TM))
                )
              (let
                (
                  (type_sym (car registry_cons))
                  (def_list (cdr registry_cons))
                  )
                (let
                  (
                    (detect_lambda (nth 1 def_list))
                    (display_lambda (nth 2 def_list))
                    (lookahead_TM (RT·TM·entangled-copy target_TM))
                    )
                  (let
                    (
                      (detect_result (funcall detect_lambda lookahead_TM))
                      )
                    (let
                      (
                        (is_valid (nth 0 detect_result))
                        (ov (nth 1 detect_result))
                        (payload_TM (nth 2 detect_result))
                        (skip_count (nth 3 detect_result))
                        )
                      (if is_valid
                        (progn
                          (setq found-nested_TM payload_TM)
                          (RT·BreadBox·introspection·write-if 
                            'RT·BreadBox·buffer·scan-tape 
                            (format "Detected type %s" type_sym)
                            )
                          (overlay-put ov 'RT·BreadBox·type type_sym)
                          
                          (if display_lambda
                            (funcall display_lambda ov nil theme_alist)
                            )
                            
                          ;; Recursive internal scan
                          (RT·BreadBox·buffer·scan-tape payload_TM type_alist theme_alist)
                          
                          ;; Advance the parent tape past the parsed structure
                          (let ((skips skip_count))
                            (while
                              (if (or (<= skips 0) (RT·TM·on-rightmost target_TM))
                                nil
                                (progn
                                  (RT·TM·step target_TM)
                                  (setq skips (1- skips))
                                  t
                                  ))))
                          ))
                      
                      ;; Center break logic for the type_TM iteration
                      (if (RT·TM·on-rightmost type_TM)
                        nil ;; Terminate inner loop
                        (progn
                          (RT·TM·step type_TM)
                          t   ;; Continue inner loop
                          ))
                      )))))))
        
        ;; Center break logic for the target_TM iteration
        (if (RT·TM·on-rightmost target_TM)
          nil ;; Terminate outer loop
          (progn
            (if (not found-nested_TM)
              (RT·TM·step target_TM)
              )
            t   ;; Continue outer loop
            ))
        )))

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
     Returns '(is_valid overlay payload_TM skip_count)"
    (let
      (
        (start_val (funcall (cdr (assq 'read lookahead_TM))))
        )
      (if
        (eq start_val ?\e)
        (let
          (
            (leftmost_pos (funcall (cdr (assq 'get-pos lookahead_TM))))
            (found-end_bool nil)
            (skip_count 1)
            )
          (RT·BreadBox·introspection·write-if 'RT·BreadBox·example-escape-literal·detect-esc-hex "Leftmost ESC found.")
          (funcall (cdr (assq 'step lookahead_TM)))
          
          (while (progn
            (if
              (funcall (cdr (assq 'on-rightmost lookahead_TM)))
              nil
              (let
                (
                  (next_val (funcall (cdr (assq 'read lookahead_TM))))
                  )
                (setq skip_count (1+ skip_count))
                (if
                  (eq next_val ?\e)
                  (progn
                    (setq found-end_bool t)
                    nil
                    )
                  (progn
                    (funcall (cdr (assq 'step lookahead_TM)))
                    t
                    )
                  )))))
                  
          (if
            found-end_bool
            (let
              (
                (rightmost-right-neighbor_pos (1+ (funcall (cdr (assq 'get-pos lookahead_TM)))))
                )
              (RT·BreadBox·introspection·write-if 'RT·BreadBox·example-escape-literal·detect-esc-hex "Rightmost ESC found.")
              (let
                (
                  (rightmost_pos (1- rightmost-right-neighbor_pos))
                  (ov (RT·BreadBox·overlay·make leftmost_pos (1- rightmost-right-neighbor_pos)))
                  (payload_TM (RT·TM·make-buffer-tape (1+ leftmost_pos) (1- rightmost_pos) (1+ leftmost_pos)))
                  )
                (list t ov payload_TM skip_count)
                ))
            (list nil nil nil nil)
            ))
        (list nil nil nil nil)
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

