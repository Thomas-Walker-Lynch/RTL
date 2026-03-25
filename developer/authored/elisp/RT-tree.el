;;; RT-tree.el --- Stage 1 Binary AST Serialization

;;;------------------------------------------------------------------------------
;;; Bit Stream Writer
;;;

  (defvar RT-tree·write-buffer nil)
  (defvar RT-tree·write-bit-count 0)
  (defvar RT-tree·write-current-byte 0)
  (defvar RT-tree·is-absolute-first t)

  (defun RT-tree·init-writer ()
    (setq RT-tree·write-buffer nil)
    (setq RT-tree·write-bit-count 0)
    (setq RT-tree·write-current-byte 0)
    (setq RT-tree·is-absolute-first t)
    )

  (defun RT-tree·push-bit (b)
    (setq RT-tree·write-current-byte
      (logior (lsh RT-tree·write-current-byte 1) b)
      )
    (setq RT-tree·write-bit-count (+ RT-tree·write-bit-count 1))
    (if 
      (= RT-tree·write-bit-count 8)
      (progn
        (push RT-tree·write-current-byte RT-tree·write-buffer)
        (setq RT-tree·write-current-byte 0)
        (setq RT-tree·write-bit-count 0)
        )))

  (defun RT-tree·push-bits (val width)
    (let 
      (
        (i (- width 1))
        )
      (while (>= i 0)
        (RT-tree·push-bit (logand (lsh val (- i)) 1))
        (setq i (- i 1))
        )))

  (defun RT-tree·finalize-writer ()
    (if 
      (> RT-tree·write-bit-count 0)
      (let 
        (
          (pad (- 8 RT-tree·write-bit-count))
          )
        (setq RT-tree·write-current-byte (lsh RT-tree·write-current-byte pad))
        (push RT-tree·write-current-byte RT-tree·write-buffer)
        ))
    (apply 'unibyte-string (nreverse RT-tree·write-buffer))
    )

;;;------------------------------------------------------------------------------
;;; Bit Stream Reader
;;;

  (defvar RT-tree·read-buffer "")
  (defvar RT-tree·read-byte-index 0)
  (defvar RT-tree·read-bit-index 0)
  (defvar RT-tree·read-len 0)

  (defun RT-tree·init-reader (bytes-in)
    (setq RT-tree·read-buffer bytes-in)
    (setq RT-tree·read-byte-index 0)
    (setq RT-tree·read-bit-index 7)
    (setq RT-tree·read-len (length bytes-in))
    )

  (defun RT-tree·read-bit ()
    (if 
      (>= RT-tree·read-byte-index RT-tree·read-len)
      nil
      (let* (
          (byte-val (aref RT-tree·read-buffer RT-tree·read-byte-index))
          (bit-val (logand (lsh byte-val (- RT-tree·read-bit-index)) 1))
          )
        (setq RT-tree·read-bit-index (- RT-tree·read-bit-index 1))
        (if 
          (< RT-tree·read-bit-index 0)
          (progn
            (setq RT-tree·read-bit-index 7)
            (setq RT-tree·read-byte-index (+ RT-tree·read-byte-index 1))
            ))
        bit-val
        )))

  (defun RT-tree·read-bits (width)
    (let 
      (
        (val 0)
        )
      (dotimes (_ width)
        (let 
          (
            (b (RT-tree·read-bit))
            )
          (if 
            b
            (setq val (logior (lsh val 1) b))
            (setq val (lsh val 1))
            )))
      val
      ))

;;;------------------------------------------------------------------------------
;;; Core Codec
;;;

  (defun RT-tree·encode-node (lst is-root parent-is-last tile-size)
    (let 
      (
        (len (length lst))
        )
      (dotimes (i len)
        (let* (
            (item (nth i lst))
            (is-first (= i 0))
            (is-last (= i (- len 1)))
            )
          (if 
            (integerp item)
            (progn
              (cond
                (RT-tree·is-absolute-first
                  (setq RT-tree·is-absolute-first nil)
                  )
                (is-first
                  (if 
                    parent-is-last
                    (RT-tree·push-bits #b111 3)
                    (RT-tree·push-bits #b10 2)
                    ))
                ;; Enforce 110 on root's last tile to terminate before padding
                (is-last
                  (RT-tree·push-bits #b110 3)
                  )
                (t
                  (RT-tree·push-bits #b0 1)
                  ))
              (RT-tree·push-bits item tile-size)
              )
            ;; is nested list
            (RT-tree·encode-node item nil is-last tile-size)
            )))))

;;;------------------------------------------------------------------------------
;;; API
;;;

  (defun RT-literal·encode (header-ast extent-ast)
    (RT-tree·init-writer)
    
    ;; 1. The 6-Bit Bootstrap Config 
    ;; Bit order: Endian(0), Traversal(1=DFS), NullHeader(0), NullPayload(0), CustomTile(0), CustomAlign(0)
    ;; 010000 binary -> #b010000
    (RT-tree·push-bits #b010000 6)
    
    ;; 2. Stage 1: The Header Tree (5-bit tiles)
    (RT-tree·encode-node header-ast t t 5)
    
    ;; 3. Stage 2: The Extent Tree (8-bit tiles)
    (RT-tree·encode-node extent-ast t t 8)
    
    ;; 4. (Payload would be appended here)

    ;; 5. Pad to byte boundary and export
    (RT-tree·finalize-writer)
    )

  (defun RT-tree·decode (binary-in tile-size)
    "Decodes a unibyte string bitstream back into an AST list."
    (RT-tree·init-reader binary-in)
    (let* (
        (first-tile (RT-tree·read-bits tile-size))
        (root (list first-tile))
        (curr root)
        (stack nil)
        (eof nil)
        )
      (while (and curr (not eof) (< RT-tree·read-byte-index RT-tree·read-len))
        (let 
          (
            (b1 (RT-tree·read-bit))
            )
          (if 
            (null b1)

            (setq eof t)

            (let 
              (
                (ctrl 0)
                )
              (if 
                (= b1 0)

                (setq ctrl 0)

                (let 
                  (
                    (b2 (RT-tree·read-bit))
                    )
                  (if 
                    (= b2 0)

                    (setq ctrl #b10)

                    (let 
                      (
                        (b3 (RT-tree·read-bit))
                        )
                      (if 
                        (= b3 0)

                        (setq ctrl #b110)

                        (setq ctrl #b111)
                        )))))

              (let 
                (
                  (tile (RT-tree·read-bits tile-size))
                  )
                (cond
                  ((= ctrl 0)
                    (nconc curr (list tile))
                    )
                  ((= ctrl #b10)
                    (let 
                      (
                        (new-list (list tile))
                        )
                      (nconc curr (list new-list))
                      (push curr stack)
                      (setq curr new-list)
                      ))
                  ((= ctrl #b110)
                    (nconc curr (list tile))
                    (setq curr (pop stack))
                    )
                  ((= ctrl #b111)
                    (let 
                      (
                        (new-list (list tile))
                        )
                      (nconc curr (list new-list))
                      (setq curr new-list)
                      ))))))))
      root
      ))

  (defun RT-tree·debug-print (binary-in tile-size)
    "Returns a formatted string of the bitstream execution trace."
    (RT-tree·init-reader binary-in)
    (let 
      (
        (output "")
        (first-tile (RT-tree·read-bits tile-size))
        (eof nil)
        (depth 0)
        )
      (setq output (concat output (format "    | %d\n" first-tile)))
      (while (and (not eof) (>= depth 0) (< RT-tree·read-byte-index RT-tree·read-len))
        (let 
          (
            (b1 (RT-tree·read-bit))
            )
          (if 
            (null b1)

            (setq eof t)

            (let 
              (
                (ctrl-text "")
                (ctrl-val 0)
                )
              (if 
                (= b1 0)

                (progn 
                  (setq ctrl-text "  0") 
                  (setq ctrl-val 0)
                  )

                (let 
                  (
                    (b2 (RT-tree·read-bit))
                    )
                  (if 
                    (= b2 0)

                    (progn 
                      (setq ctrl-text " 10") 
                      (setq ctrl-val #b10)
                      )

                    (let 
                      (
                        (b3 (RT-tree·read-bit))
                        )
                      (if 
                        (= b3 0)

                        (progn 
                          (setq ctrl-text "110") 
                          (setq ctrl-val #b110)
                          )

                        (progn 
                          (setq ctrl-text "111") 
                          (setq ctrl-val #b111)
                          ))))))

              (let 
                (
                  (tile (RT-tree·read-bits tile-size))
                  )
                (setq output (concat output (format "%s | %d\n" ctrl-text tile)))
                (cond
                  ((= ctrl-val #b10) 
                    (setq depth (+ depth 1))
                    )
                  ((= ctrl-val #b110) 
                    (setq depth (- depth 1))
                    )))))))
      output
      ))
