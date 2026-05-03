;;;=============================================================================
;;; ExampleQuotedBinary
;;;

;;;-----------------------------------------------------------------------------
;;; Configuration
;;;

  (defconst RT·ExampleQuotedBinary·list_interface
    '(
       ExampleQuotedBinary
       RT·ExampleQuotedBinary·make       
       RT·ExampleQuotedBinary·make-if    
       RT·ExampleQuotedBinary·display
       nil 
       nil 
       t
       )
    "The canonical interface definition for ExampleQuotedBinary.
     Slots: (type make make-if display edit has-right-neighbor can-be-nested)"
    )

;;;-----------------------------------------------------------------------------
;;; Interior code
;;;

  ;; (Reserved for private helpers mapping structural bytes if needed)

;;;-----------------------------------------------------------------------------
;;; API
;;;

  (defun RT·ExampleQuotedBinary·make (buffer_byte buffer_display byte_payload-leftmost byte_payload-rightmost encoding_character)
    "Instantiate a TM strictly over the payload bounds."
    (RT·TM·buffer-byte·make 
      buffer_byte 
      buffer_display 
      byte_payload-leftmost 
      byte_payload-rightmost 
      byte_payload-leftmost 
      encoding_character
      ))

  (defun RT·ExampleQuotedBinary·make-if (tm_substrate encoding_character)
    "Attempt to make a TM if the d“ prefix and ”b suffix match within the byte buffer."
    (let
      (
        (tm_prefix (RT·TM·string-byte·make "d“" encoding_character))
        (tm_suffix (RT·TM·string-byte·make "”b" encoding_character))
        )
      (if
        (RT·TM·sequence·eq tm_substrate tm_prefix)
        (let*
          (
            (byte_ov-leftmost (RT·TM·get-pos tm_substrate))
            (tm_lookahead-byte (RT·TM·entangled-copy tm_substrate))
            (int_prefix-len-bytes (length (encode-coding-string "d“" encoding_character)))
            (int_suffix-len-bytes (length (encode-coding-string "”b" encoding_character)))
            )
          
          (let ((int_skips int_prefix-len-bytes))
            (while (> int_skips 0)
              (RT·TM·step tm_lookahead-byte)
              (setq int_skips (1- int_skips))
              ))
          
          (let*
            (
              (byte_payload-leftmost (RT·TM·get-pos tm_lookahead-byte))
              (buffer_byte (RT·TM·buffer-byte tm_substrate))
              (buffer_display (RT·TM·buffer-display tm_substrate))
              (byte_max (with-current-buffer buffer_byte (point-max)))
              (tm_scan (RT·TM·buffer-byte·make buffer_byte buffer_display byte_payload-leftmost byte_max byte_payload-leftmost encoding_character))
              )
            
            (let
              (
                (bool_found-end
                  (catch 'found
                    (while
                      (if (not (RT·TM·has-right-neighbor tm_scan))
                        nil
                        (if (RT·TM·sequence·eq tm_scan tm_suffix)
                          (throw 'found t)
                          (progn
                            (RT·TM·step tm_scan)
                            t
                            ))))
                    nil
                    ))
                )
              (if bool_found-end
                (let*
                  (
                    (byte_payload-rightmost (1- (RT·TM·get-pos tm_scan)))
                    (byte_ov-rightmost (+ byte_payload-rightmost int_suffix-len-bytes))
                    (ov (RT·TM·overlay·project buffer_display byte_ov-leftmost byte_ov-rightmost))
                    
                    ;; The detector isolates the boundaries, but the instantiator builds the TM
                    (tm_payload (RT·ExampleQuotedBinary·make buffer_byte buffer_display byte_payload-leftmost byte_payload-rightmost encoding_character))
                    )
                  
                  (overlay-put ov 'RT·TM-type 'ExampleQuotedBinary)
                  (overlay-put ov 'RT·TM tm_payload)
                  (overlay-put ov 'RT·byte-rightmost byte_ov-rightmost)
                  
                  (setcdr (last tm_payload) (list (cons 'overlay (lambda () ov))))
                  
                  tm_payload
                  )
                nil
                ))))
        nil
        )))

  (defun RT·ExampleQuotedBinary·display (ov tm_payload alist_theme)
    "Applies visual highlighting to the overlay in buffer-0."
    (overlay-put ov 'face '(:background "gray15" :foreground "light green"))
    )

  (defun RT·ExampleQuotedBinary·setup ()
    (add-to-list 'RT·TM·list_make-if 'RT·ExampleQuotedBinary·make-if)
    )


;;;-----------------------------------------------------------------------------
;;; Integration
;;;

  ;; Register the canonical interface to the core engine
  (RT·TM·type·register 'ExampleQuotedBinary RT·ExampleQuotedBinary·list_interface)

  ;; Add the detector to the introspection logging path
  (push 'RT·ExampleQuotedBinary·make-if RT·TM·list_symbol-introspection)
