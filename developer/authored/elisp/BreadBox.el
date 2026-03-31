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
;;;   `edit` is externally defined. One possibility is that it opens another panel for editing the BreadBox contents in another panel. Some BreadBoxes can be nested, so if a user opens a new BreadBox or edits a contained one, the current edit is pushed on to the buffer's RT-BreadBox·overlay·stack. Then editing of the outer BreadBox can continue after the inner BreadBox is finished.
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

;;;-----------------------------------------------------------------------------
;;; Configuration
;;;

  ;; decoration
  ;;

  (defvar-local RT-BreadBox·overlay·background 'RT-BreadBox·overlay·highlight_shade)
  (defvar-local RT-BreadBox·overlay·selected    'RT-BreadBox·overlay·highlight_box)

  ;; overlay stack
  ;;

  (defvar-local RT-BreadBox·overlay·stack nil)
  (defvar-local RT-BreadBox·overlay·stack·selected nil)

  ;; display
  ;;

  (defvar RT-BreadBox·display·wrap-indent 2)

;;;-----------------------------------------------------------------------------
;;; Interior code
;;;

  ;; overlay
  ;;

  (defun RT-BreadBox·overlay·make (leftmost_pos rightmost_pos))

  (defun RT-BreadBox·overlay·wrap-on ())
  (defun RT-BreadBox·overlay·wrap-off ())

  (defmacro RT-BreadBox·overlay·leftmost (overlay)
    `(overlay-start ,overlay)
    )

  (defmacro RT-BreadBox·overlay·rightmost (overlay))

  (defmacro RT-BreadBox·overlay·rightmost-right-neighbor (overlay)
    `(overlay-end ,overlay)
    )

  (defun RT-BreadBox·overlay·stack-is-empty ()
    (RT-BreadBox·stack·stack-is-empty RT-BreadBox·stack·overlay-stack)
    )

  (defun RT-BreadBox·overlay·highlight-none ()
    nil
    )

  (defun RT-BreadBox·overlay·highlight-default ()
    'region
    )

  (defun RT-BreadBox·overlay·highlight-shades ()
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




;;;-----------------------------------------------------------------------------
;;; API
;;;

  (defun RT-BreadBox·buffer·insert-binary-payload-with-overlay (binary-data display-lambda)
    "Insert BINARY-DATA at point, cover it with an overlay, and call DISPLAY-LAMBDA."
    (let ((start-pos (point)))
      ;; Insert the raw unibyte string directly into the buffer
      (insert binary-data)
      (let* ((end-pos (point))
             ;; Create the overlay spanning the newly inserted bytes
             (payload-overlay (make-overlay start-pos end-pos)))

        ;; Pass the overlay and the data to the provided lambda
        (funcall display-lambda payload-overlay binary-data))))

      ;; example
      ;; (let ((my-image-data (unibyte-string #xFF #xD8 #xFF #xE0)))
      ;;   (insert-binary-payload-with-overlay
      ;;    my-image-data
      ;;    (lambda (ov data)
      ;;      (let ((size (length data)))
      ;;        ;; Visually replace the raw bytes with a clean label
      ;;        (overlay-put ov 'display (format "[IMAGE PAYLOAD: %d bytes]" size))))))

  (defun RT-BreadBox·buffer·update-binary-backing (ov new-binary-data new-display-string)
    "Replace the binary data under OV with NEW-BINARY-DATA, and update display."
    (let ((start (overlay-start ov))
           (old-end (overlay-end ov)))
      (save-excursion
        (goto-char start)
        ;; 1. Insert the new data first to prevent the overlay from collapsing
        (insert new-binary-data)

        (let ((new-end (point)))
          ;; 2. Delete the old binary data.
          ;; The old data shifted forward by the length of the new insertion.
          (delete-region new-end (+ new-end (- old-end start)))

          ;; 3. Ensure the overlay bounds match the newly inserted data
          (move-overlay ov start new-end)

          ;; 4. Update the visual display to reflect the user's edits
          (overlay-put ov 'display new-display-string)))))

  (defun RT-BreadBox·buffer·create-interactive-payload-overlay (start end display-text label)
    "Create an overlay from START to END with a clickable DISPLAY-TEXT."
    (let ((ov (make-overlay start end))
          (map (make-sparse-keymap)))

      ;; Define the actions for clicking or pressing Enter
      (define-key map [mouse-1] 'my-trigger-edit-function)
      (define-key map (kbd "RET") 'my-trigger-edit-function)

      ;; Apply the properties
      (overlay-put ov 'display display-text)
      (overlay-put ov 'keymap map)
      ;; Change the mouse cursor to indicate it is interactive
      (overlay-put ov 'pointer 'hand)

      ov))



;;;-----------------------------------------------------------------------------
;;; Interactive
;;;



;;;-----------------------------------------------------------------------------
;;; Integration
;;;


;;;-----------------------------------------------------------------------------
;;; Example
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


  ;; example find-next lambda
  ;; caref
  ;; finds the next interesting binary field
  ;; returns (leftmost_pos rightmost_pos)
  (defun RT-BreadBox·buffer·find-next ())

; LocalWords:  BreadBox
