;;; First Order List
;;;
;;;   A first order list holds a bit array in a controlled sequence of tiles.
;;;   Control takes the form of a bit that says if a given tile has a right neighbor or not.


;;;------------------------------------------------------------------------------
;;; Utilities
;;;


;;;-----------------------------------------------------------------------------
;;; Configuration
;;;

  (defconst RT·first-order-list·control·continue 0)
  (defconst RT·first-order-list·control·terminate 1)

;;;-----------------------------------------------------------------------------
;;; Interior code
;;;

  ;; Converts a bit array to a space-separated string for aligned column printing.
  (defun RT·bit-array·to-string (bit-array)
    (let
      (
        (count (length bit-array))
        (i 0)
        (str "")
        )
      (while
        (< i count)
        (setq str (concat str (format "%d " (aref bit-array i))))
        (setq i (+ i 1))
        )
      str
      ))

;;;-----------------------------------------------------------------------------
;;; API
;;;

(defun RT·first-order-list·encode (tile_size input_bit-array)
    (let
      (
        (input_count (length input_bit-array))
        )
      (if
        (= input_count 0)
        'RT·first-order-list·status·empty-input
        (let*
          (
            (tile_count (/ (+ input_count (- tile_size 1)) tile_size))
            (padded_count (* tile_count tile_size))
            (output_count (+ padded_count tile_count))
            (output_bit-array (make-vector output_count 0))
            (in_i 0)
            (out_i 0)
            (tile_i 0)
            )
          (while
            (< tile_i tile_count)
            (let
              (
                (is-last_bool (= tile_i (- tile_count 1)))
                (bit_i 0)
                )
              (if
                is-last_bool
                (aset output_bit-array out_i RT·first-order-list·control·terminate)
                (aset output_bit-array out_i RT·first-order-list·control·continue)
                )
              (setq out_i (+ out_i 1))
              (while
                (< bit_i tile_size)
                (let
                  (
                    (bit_val
                      (if
                        (< in_i input_count)
                        (aref input_bit-array in_i)
                        0
                        )))
                  (aset output_bit-array out_i bit_val)
                  (setq out_i (+ out_i 1))
                  (setq in_i (+ in_i 1))
                  (setq bit_i (+ bit_i 1))
                  ))
              (setq tile_i (+ tile_i 1))
              ))
          (let
            (
              (extent_i (- output_count 1))
              )
            (cons extent_i output_bit-array)
            )))))

  ;; Decodes a controlled first-order list bit array back into its raw data tiles.
  (defun RT·first-order-list·decode (tile_size input_bit-array)
    (let*
      (
        (input_count (length input_bit-array))
        (pair_size (+ tile_size 1))
        (tile_count
          (if
            (= input_count 0)
            0
            (/ input_count pair_size)
            ))
        (output_count (* tile_count tile_size))
        (output_bit-array (make-vector output_count 0))
        (in_i 0)
        (out_i 0)
        (tile_i 0)
      )
      (while
        (< tile_i tile_count)
        (let
          (
            (bit_i 0)
          )
          (setq in_i (+ in_i 1)) ; Skip the control bit
          (while
            (< bit_i tile_size)
            (aset output_bit-array out_i (aref input_bit-array in_i))
            (setq out_i (+ out_i 1))
            (setq in_i (+ in_i 1))
            (setq bit_i (+ bit_i 1))
            )
          (setq tile_i (+ tile_i 1))
          ))
      output_bit-array
      ))

  ;; Compares two bit arrays, returning an array with 0 for match, 1 for mismatch.
  (defun RT·bit-array·diff (array-a array-b)
    (let*
      (
        (count-a (length array-a))
        (count-b (length array-b))
        (max-count
          (if
            (> count-a count-b)
            count-a
            count-b
            )
          )
        (diff_array (make-vector max-count 0))
        (i 0)
      )
      (while
        (< i max-count)
        (let
          (
            (has-a_bool (< i count-a))
            (has-b_bool (< i count-b))
            (val-a -1)
            (val-b -2)
          )
          (if
            has-a_bool
            (setq val-a (aref array-a i))
            nil
            )
          (if
            has-b_bool
            (setq val-b (aref array-b i))
            nil
            )
          (if
            (= val-a val-b)
            (aset diff_array i 0)
            (aset diff_array i 1)
            )
          (setq i (+ i 1))
          ))
      diff_array
      ))

  ;; By contract: `encoded_bit-array` must be a valid first-order list encoding.
  ;; Writes bits to the buffer at point using LSB-first packing.
  ;; Scans until the termination bit and its tile are written, returning the encoded extent.
  (defun RT·first-order-list·buffer·write (tile_size encoded_bit-array)
    (let
      (
        (input_count (length encoded_bit-array))
        (in_i 0)
        (byte_val 0)
        (bit_i 0)
        (terminated_bool nil)
        (error_sym nil)
        )
      (while
        (and (not terminated_bool) (not error_sym))
        (if
          (>= in_i input_count)
          (setq error_sym 'RT·first-order-list·status·unexpected-end-of-array)
          (let
            (
              (control_bit (aref encoded_bit-array in_i))
              (tile_bit_count 0)
              )
            (setq in_i (+ in_i 1))
            (if
              (= control_bit 1)
              (setq terminated_bool t)
              nil
              )
            (setq byte_val (logior byte_val (lsh control_bit bit_i)))
            (setq bit_i (+ bit_i 1))
            (if
              (= bit_i 8)
              (progn
                (insert byte_val)
                (setq byte_val 0)
                (setq bit_i 0)
                )
              nil
              )
            (while
              (and (< tile_bit_count tile_size) (not error_sym))
              (if
                (>= in_i input_count)
                (setq error_sym 'RT·first-order-list·status·unexpected-end-of-array)
                (let
                  (
                    (tile_bit (aref encoded_bit-array in_i))
                    )
                  (setq in_i (+ in_i 1))
                  (setq byte_val (logior byte_val (lsh tile_bit bit_i)))
                  (setq bit_i (+ bit_i 1))
                  (if
                    (= bit_i 8)
                    (progn
                      (insert byte_val)
                      (setq byte_val 0)
                      (setq bit_i 0)
                      )
                    nil
                    )
                  (setq tile_bit_count (+ tile_bit_count 1))
                  )))
            )))
      (if
        error_sym
        error_sym
        (progn
          (if
            (> bit_i 0)
            (insert byte_val)
            nil
            )
          (- in_i 1)
          ))))

  ;; Reads a first-order list encoding from the buffer at point, decoding it
  ;; on the fly until the terminator bit and its tile are processed.
  ;; Expects LSB-first bit packing. Returns (cons extent decoded_bit-array).
  (defun RT·first-order-list·buffer·read (tile_size)
    (let
      (
        (byte_val 0)
        (bit_i 8)
        (terminated_bool nil)
        (error_sym nil)
        (decoded_list nil)
        )
      (while
        (and (not terminated_bool) (not error_sym))
        (if
          (= bit_i 8)
          (if
            (eobp)
            (setq error_sym 'RT·first-order-list·status·unexpected-eof)
            (progn
              (setq byte_val (char-after (point)))
              (forward-char 1)
              (setq bit_i 0)
              ))
          nil
          )
        (if
          (not error_sym)
          (let
            (
              (control_bit (logand (lsh byte_val bit_i) 1))
              (tile_bit_count 0)
              )
            (setq bit_i (+ bit_i 1))
            (if
              (= control_bit 1)
              (setq terminated_bool t)
              nil
              )
            (while
              (and (< tile_bit_count tile_size) (not error_sym))
              (if
                (= bit_i 8)
                (if
                  (eobp)
                  (setq error_sym 'RT·first-order-list·status·unexpected-eof)
                  (progn
                    (setq byte_val (char-after (point)))
                    (forward-char 1)
                    (setq bit_i 0)
                    ))
                nil
                )
              (if
                (not error_sym)
                (let
                  (
                    (tile_bit (logand (lsh byte_val bit_i) 1))
                    )
                  (setq bit_i (+ bit_i 1))
                  (setq decoded_list (cons tile_bit decoded_list))
                  (setq tile_bit_count (+ tile_bit_count 1))
                  )
                nil
                ))
            )
          nil
          ))
      (if
        error_sym
        error_sym
        (let*
          (
            (decoded_array (vconcat (nreverse decoded_list)))
            (extent_i (- (length decoded_array) 1))
            )
          (cons extent_i decoded_array)
          ))))

  ;; By contract: `encoded_bit-array` must be a valid first-order list encoding.
  ;; Pre-calculates exact bit requirements to prevent mid-overlay write failures.
  (defun RT·first-order-list·overlay·write (target_overlay offset tile_size encoded_bit-array)
    (let
      (
        (in_i 0)
        (input_count (length encoded_bit-array))
        (terminated_bool nil)
        (error_sym nil)
        )
      (while
        (and (not terminated_bool) (not error_sym))
        (if
          (>= in_i input_count)
          (setq error_sym 'RT·first-order-list·status·unexpected-end-of-array)
          (let
            (
              (control_bit (aref encoded_bit-array in_i))
              )
            (if
              (= control_bit 1)
              (setq terminated_bool t)
              nil
              )
            (setq in_i (+ in_i 1 tile_size))
            (if
              (> in_i input_count)
              (setq error_sym 'RT·first-order-list·status·unexpected-end-of-array)
              nil
              ))))
      (if
        error_sym
        error_sym
        (let*
          (
            (required_bits in_i)
            (required_bytes (/ (+ required_bits 7) 8))
            (start_pos (+ (overlay-start target_overlay) offset))
            (available_bytes (- (overlay-end target_overlay) start_pos))
            (is-expandable_bool (overlay-get target_overlay 'RT-expandable_bool))
            )
          (if
            (and (> required_bytes available_bytes) (not is-expandable_bool))
            'RT·first-order-list·status·overlay-overflow
            (let
              (
                (original_point (point))
                (byte_val 0)
                (bit_i 0)
                (read_i 0)
                )
              (goto-char start_pos)
              (while
                (< read_i required_bits)
                (let
                  (
                    (bit (aref encoded_bit-array read_i))
                    )
                  (setq byte_val (logior byte_val (lsh bit bit_i)))
                  (setq bit_i (+ bit_i 1))
                  (if
                    (= bit_i 8)
                    (progn
                      (if
                        (< (point) (overlay-end target_overlay))
                        (progn
                          (delete-char 1)
                          (insert byte_val)
                          )
                        (progn
                          (insert byte_val)
                          (move-overlay target_overlay (overlay-start target_overlay) (point))
                          ))
                      (setq byte_val 0)
                      (setq bit_i 0)
                      )
                    nil
                    )
                  (setq read_i (+ read_i 1))
                  ))
              (if
                (> bit_i 0)
                (if
                  (< (point) (overlay-end target_overlay))
                  (progn
                    (delete-char 1)
                    (insert byte_val)
                    )
                  (progn
                    (insert byte_val)
                    (move-overlay target_overlay (overlay-start target_overlay) (point))
                    ))
                nil
                )
              (goto-char original_point)
              (- required_bits 1)
              ))))))

  ;; Reads a first-order list encoding from an overlay, decoding on the fly.
  (defun RT·first-order-list·overlay·read (target_overlay offset tile_size)
    (let*
      (
        (start_pos (+ (overlay-start target_overlay) offset))
        (overlay_end (overlay-end target_overlay))
        (original_point (point))
        )
      (goto-char start_pos)
      (let
        (
          (byte_val 0)
          (bit_i 8)
          (terminated_bool nil)
          (error_sym nil)
          (decoded_list nil)
          )
        (while
          (and (not terminated_bool) (not error_sym))
          (if
            (= bit_i 8)
            (if
              (>= (point) overlay_end)
              (setq error_sym 'RT·first-order-list·status·overlay-out-of-bounds)
              (progn
                (setq byte_val (char-after (point)))
                (forward-char 1)
                (setq bit_i 0)
                ))
            nil
            )
          (if
            (not error_sym)
            (let
              (
                (control_bit (logand (lsh byte_val bit_i) 1))
                (tile_bit_count 0)
                )
              (setq bit_i (+ bit_i 1))
              (if
                (= control_bit 1)
                (setq terminated_bool t)
                nil
                )
              (while
                (and (< tile_bit_count tile_size) (not error_sym))
                (if
                  (= bit_i 8)
                  (if
                    (>= (point) overlay_end)
                    (setq error_sym 'RT·first-order-list·status·overlay-out-of-bounds)
                    (progn
                      (setq byte_val (char-after (point)))
                      (forward-char 1)
                      (setq bit_i 0)
                      ))
                  nil
                  )
                (if
                  (not error_sym)
                  (let
                    (
                      (tile_bit (logand (lsh byte_val bit_i) 1))
                      )
                    (setq bit_i (+ bit_i 1))
                    (setq decoded_list (cons tile_bit decoded_list))
                    (setq tile_bit_count (+ tile_bit_count 1))
                    )
                  nil
                  ))
              )
            nil
            ))
        (goto-char original_point)
        (if
          error_sym
          error_sym
          (let*
            (
              (decoded_array (vconcat (nreverse decoded_list)))
              (extent_i (- (length decoded_array) 1))
              )
            (cons extent_i decoded_array)
            )))))


;;;-----------------------------------------------------------------------------
;;; Interactive
;;;

  (defun RT·first-order-list·example-0 ()
    "Generates 7 sample encodings into the current buffer."
    (interactive)
    (let
      (
        (example-1 '(4 [0]))
        (example-2 '(5 [1]))
        (example-3 '(6 []))
        (example-4 '(4 [1 0 1 1]))
        (example-5 '(4 [1 1 0 0 0 0 1 1 1 0 1 0]))
        (example-6 '(5 [1 1 1]))
        (example-7 '(5 [0 0 0 0 1]))
        )
      (insert (format "%S\n" (apply 'RT·first-order-list·encode example-1)))
      (insert (format "%S\n" (apply 'RT·first-order-list·encode example-2)))
      (insert (format "%S\n" (apply 'RT·first-order-list·encode example-3)))
      (insert (format "%S\n" (apply 'RT·first-order-list·encode example-4)))
      (insert (format "%S\n" (apply 'RT·first-order-list·encode example-5)))
      (insert (format "%S\n" (apply 'RT·first-order-list·encode example-6)))
      (insert (format "%S\n" (apply 'RT·first-order-list·encode example-7)))
      ))


  ;; Encodes, decodes, and diffs sample bit arrays, printing the aligned results.
  (defun RT·first-order-list·example-1 ()
    (interactive)
    (let
      (
        (example_list
          (list
            '(4 [0])
            '(5 [1])
            '(6 [])
            '(4 [1 0 1 1])
            '(4 [1 1 0 0 0 0 1 1 1 0 1 0])
            '(5 [1 1 1])
            '(5 [0 0 0 0 1])
            )))
      (mapc
        (lambda (ex)
          (let
            (
              (tile_size (nth 0 ex))
              (original_array (nth 1 ex))
              )
            (let
              (
                (encoded_cons (RT·first-order-list·encode tile_size original_array))
                )
              (insert (format "Tile Size: %d\n" tile_size))
              (insert (format "Original: %s\n" (RT·bit-array·to-string original_array)))
              (if
                (eq encoded_cons 'RT·first-order-list·status·empty-input)
                (insert "Status:   Cannot encode empty bit array.\n\n")
                (let*
                  (
                    (encoded_array (cdr encoded_cons))
                    (decoded_array (RT·first-order-list·decode tile_size encoded_array))
                    (diff_array (RT·bit-array·diff original_array decoded_array))
                    )
                  (insert (format "Decoded:  %s\n" (RT·bit-array·to-string decoded_array)))
                  (insert (format "Diff:     %s\n\n" (RT·bit-array·to-string diff_array)))
                  )))))
        example_list
        )))

  ;; Validates the streaming decode/write logic LSB first, and tests overlay bounds.
  (defun RT·first-order-list·example-2 ()
    (interactive)
    (let
      (
        (example_list
          (list
            '(4 [1 0 1 1])
            '(4 [1 1 0 0 0 0 1 1 1 0 1 0])
            '(5 [1 1 1])
            )))
      (insert "=== Buffer Read/Write (LSB-First Decoder) ===\n")
      (mapc
        (lambda (ex)
          (let
            (
              (tile_size (nth 0 ex))
              (original_array (nth 1 ex))
              )
            (let*
              (
                (encoded_cons (RT·first-order-list·encode tile_size original_array))
                (encoded_array (cdr encoded_cons))
                (write_start (point))
                )
              (let
                (
                  (write_extent (RT·first-order-list·buffer·write tile_size encoded_array))
                  (write_end (point))
                  )
                (goto-char write_start)
                (let*
                  (
                    (read_cons (RT·first-order-list·buffer·read tile_size))
                    (read_extent (car read_cons))
                    (read_array (cdr read_cons))
                    (diff_array (RT·bit-array·diff original_array read_array))
                    )
                  (goto-char write_end)
                  (insert (format "\nTile Size:    %d\n" tile_size))
                  (insert (format "Original:     %s\n" (RT·bit-array·to-string original_array)))
                  (insert (format "Decoded:      %s\n" (RT·bit-array·to-string read_array)))
                  (insert (format "Diff:         %s\n" (RT·bit-array·to-string diff_array)))
                  (insert (format "Write Extent: %s\n" write_extent))
                  (insert (format "Read Extent:  %s\n\n" read_extent))
                  )))))
        example_list
        )

      (insert "=== Overlay Edge Cases ===\n")
      (let*
        (
          (tile_size 4)
          (original_array [1 1 1 1])
          (encoded_cons (RT·first-order-list·encode tile_size original_array))
          (encoded_array (cdr encoded_cons))
          (ov_start (point))
          (target_overlay (make-overlay ov_start (+ ov_start 1)))
          )
        (insert ".") ; Fixed bounds visualizer
        (overlay-put target_overlay 'RT-expandable_bool nil)
        (let
          (
            (write_status (RT·first-order-list·overlay·write target_overlay 0 tile_size encoded_array))
            )
          (insert (format "\nFixed Overlay Write Status: %S\n" write_status))
          (overlay-put target_overlay 'RT-expandable_bool t)
          (let
            (
              (expand_status (RT·first-order-list·overlay·write target_overlay 0 tile_size encoded_array))
              )
            (goto-char (overlay-end target_overlay))
            (insert (format "\nExpandable Write Status: %S\n" expand_status))
            (let
              (
                (read_cons (RT·first-order-list·overlay·read target_overlay 0 tile_size))
                )
              (insert (format "Read Decoded Extent:     %S\n\n" (car read_cons)))
              ))))))


;;;-----------------------------------------------------------------------------
;;; Integration
;;;


