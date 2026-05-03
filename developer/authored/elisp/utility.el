;;;=============================================================================
;;; Utility
;;;

;;;-----------------------------------------------------------------------------
;;; Introspection
;;;

  (defvar RT·TM·list_symbol-introspection nil)

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
;;;-----------------------------------------------------------------------------
;;; Character set
;;;

  ;; state vars
  ;;

  (defvar RT·charset·name-to-idx (RT·dict·make)
    "Dictionary mapping RT character set names to authoritative integer indices."
    )

  (defvar RT·charset·emacs-to-name (RT·dict·make)
    "Dictionary mapping native Emacs coding systems to RT names."
    )

  (defvar RT·charset·name-to-emacs (RT·dict·make)
    "Dictionary mapping RT names to native Emacs coding systems."
    )

  (defvar RT·charset·idx-to-name (make-vector 256 nil)
    "Table mapping integer indices back to RT names."
    )

  ;; Interior
  ;;

  (defun RT·charset·register-idx (list_names)
    "Populates the two way translation between RT names and integer indices."
    (let
      (
        (int_idx 0)
        (list_current list_names)
        )
      (RT·loop
        (if
          (null list_current)
          (RT·break nil)
          (let
            (
              (sym_RT-name (car list_current))
              )
            (RT·dict·write RT·charset·name-to-idx sym_RT-name int_idx)
            (aset RT·charset·idx-to-name int_idx sym_RT-name)
            (setq int_idx (1+ int_idx))
            (setq list_current (cdr list_current))
            )))))

  (defun RT·charset·register-emacs (alist_mappings)
    "Populates the two way translation between Emacs names and RT names."
    (let
      (
        (list_current alist_mappings)
        )
      (RT·loop
        (if
          (null list_current)
          (RT·break nil)
          (let*
            (
              (cons_mapping (car list_current))
              (sym_RT-name (car cons_mapping))
              (sym_emacs-name (cdr cons_mapping))
              )
            (RT·dict·write RT·charset·emacs-to-name sym_emacs-name sym_RT-name)
            (RT·dict·write RT·charset·name-to-emacs sym_RT-name sym_emacs-name)
            (setq list_current (cdr list_current))
            )))))

  ;; Interface
  ;;

  (defun RT·buffer-charset·get (&optional buffer_target)
    "Returns the authoritative RT name for the buffer character encoding."
    (let*
      (
        (buffer_active (if buffer_target buffer_target (current-buffer)))
        (sym_emacs-raw (with-current-buffer buffer_active buffer-file-coding-system))
        (sym_RT-name (RT·dict·read RT·charset·emacs-to-name sym_emacs-raw))
        )
      (if
        (not sym_RT-name)
        (let
          (
            (sym_emacs-base (coding-system-base sym_emacs-raw))
            )
          (setq sym_RT-name (RT·dict·read RT·charset·emacs-to-name sym_emacs-base))
          ))
      (if
        (not sym_RT-name)
        (setq sym_RT-name 'RT·CharacterEncoding·unknown)
        )
      sym_RT-name
      ))

  (defun RT·buffer-charset·set (arg_1 &optional arg_2)
    "Sets the buffer character encoding. Accepts (RT-name) or (buffer RT-name)."
    (let*
      (
        (buffer_target (if arg_2 arg_1 (current-buffer)))
        (sym_RT-name-target (if arg_2 arg_2 arg_1))
        (sym_emacs-target (RT·dict·read RT·charset·name-to-emacs sym_RT-name-target))
        )
      (if
        sym_emacs-target
        (with-current-buffer buffer_target
          (set-buffer-file-coding-system sym_emacs-target)
          (if
            (memq sym_emacs-target '(binary raw-text-unix raw-text-dos raw-text-mac raw-text))
            (set-buffer-multibyte nil)
            (set-buffer-multibyte t)
            )
          (RT·TM·introspection·write-if 
            'RT·buffer-charset·set 
            (format "Set buffer %s to %s (%s)." (buffer-name buffer_target) sym_RT-name-target sym_emacs-target)
            ))
        (error "RT·TM: Unknown character encoding %s" sym_RT-name-target)
        )))

  ;; Integration
  ;;

  ;; Character Set Notes
  ;;
  ;; [1] Emacs displays characters with bit 7 set (values 128-255) using octal 
  ;; escape codes when using these encodings, rather than rendering graphic 
  ;; glyphs. The underlying byte values remain unchanged.
  ;;

  (defun RT·CharacterEncoding·setup ()
    (RT·charset·register-idx
      '(
         ;; 3-bit Tile Prime Real Estate (0-7)
         RT·CharacterEncoding·none
         RT·CharacterEncoding·UTF-8
         RT·CharacterEncoding·ASCII_7-bit
         RT·CharacterEncoding·first-order-7_code-point
         RT·CharacterEncoding·RT-Controlled-Data

         RT·CharacterEncoding·extended-ASCII_newline_is_LF
         RT·CharacterEncoding·extended-ASCII_newline_is_CR-LF
         RT·CharacterEncoding·extended-ASCII_newline_is_CR
         
         ;; Universal & Top Western (8-14)
         RT·CharacterEncoding·UTF-16_little-endian
         RT·CharacterEncoding·UTF-16_big-endian

         RT·CharacterEncoding·UTF-8_with-signature
         RT·CharacterEncoding·Windows-1252
         RT·CharacterEncoding·ISO-8859-1
         RT·CharacterEncoding·GB18030
         RT·CharacterEncoding·GBK

         ;; East Asian Standards (15-24)
         RT·CharacterEncoding·GB2312
         RT·CharacterEncoding·Big5
         RT·CharacterEncoding·Windows-950
         RT·CharacterEncoding·Shift-JIS
         RT·CharacterEncoding·EUC-JP

         RT·CharacterEncoding·ISO-2022-JP
         RT·CharacterEncoding·EUC-KR
         RT·CharacterEncoding·CNS-11643
         RT·CharacterEncoding·Windows-1251
         RT·CharacterEncoding·KOI8-R

         ;; Cyrillic & Major Regional (25-34)
         RT·CharacterEncoding·KOI8-U
         RT·CharacterEncoding·IBM866
         RT·CharacterEncoding·Mac-Cyrillic
         RT·CharacterEncoding·Windows-1256
         RT·CharacterEncoding·Windows-1254

         RT·CharacterEncoding·Windows-1255
         RT·CharacterEncoding·Windows-1253
         RT·CharacterEncoding·Windows-1250
         RT·CharacterEncoding·Windows-1257
         RT·CharacterEncoding·Windows-1258

         ;; Legacy & Specific ISO (35-49)
         RT·CharacterEncoding·Windows-874
         RT·CharacterEncoding·ISO-8859-2
         RT·CharacterEncoding·ISO-8859-3
         RT·CharacterEncoding·ISO-8859-4
         RT·CharacterEncoding·ISO-8859-5

         RT·CharacterEncoding·ISO-8859-6
         RT·CharacterEncoding·ISO-8859-7
         RT·CharacterEncoding·ISO-8859-8
         RT·CharacterEncoding·ISO-8859-8-I
         RT·CharacterEncoding·ISO-8859-10

         RT·CharacterEncoding·ISO-8859-13
         RT·CharacterEncoding·ISO-8859-14
         RT·CharacterEncoding·ISO-8859-15
         RT·CharacterEncoding·ISO-8859-16
         RT·CharacterEncoding·Mac-Roman

         ;; Boundary & Archival (50-59)
         RT·CharacterEncoding·CP437
         RT·CharacterEncoding·CP850
         RT·CharacterEncoding·UTF-32_little-endian
         RT·CharacterEncoding·UTF-32_big-endian
         RT·CharacterEncoding·UTF-7

         RT·CharacterEncoding·EBCDIC_US
         RT·CharacterEncoding·ASCII_7-bit_with-parity
         RT·CharacterEncoding·replacement
         RT·CharacterEncoding·x-user-defined
         RT·CharacterEncoding·unknown
         ))
    
    (RT·charset·register-emacs
      '(
         (RT·CharacterEncoding·none . binary)
         (RT·CharacterEncoding·UTF-8 . utf-8)
         (RT·CharacterEncoding·ASCII_7-bit . us-ascii)
         ;; (RT·CharacterEncoding·first-order-7_code-point . nil)
         ;; (RT·CharacterEncoding·RT-Controlled-Data . nil)

         (RT·CharacterEncoding·extended-ASCII_newline_is_LF . raw-text-unix)
         (RT·CharacterEncoding·extended-ASCII_newline_is_CR-LF . raw-text-dos)
         (RT·CharacterEncoding·extended-ASCII_newline_is_CR . raw-text-mac)
         (RT·CharacterEncoding·UTF-16_little-endian . utf-16le)
         (RT·CharacterEncoding·UTF-16_big-endian . utf-16be)

         (RT·CharacterEncoding·UTF-8_with-signature . utf-8-with-signature)
         (RT·CharacterEncoding·Windows-1252 . windows-1252)
         (RT·CharacterEncoding·ISO-8859-1 . iso-8859-1)
         (RT·CharacterEncoding·GB18030 . gb18030)
         (RT·CharacterEncoding·GBK . gbk)

         (RT·CharacterEncoding·GB2312 . gb2312)
         (RT·CharacterEncoding·Big5 . big5)
         (RT·CharacterEncoding·Windows-950 . cp950)
         (RT·CharacterEncoding·Shift-JIS . shift_jis)
         (RT·CharacterEncoding·EUC-JP . euc-jp)

         (RT·CharacterEncoding·ISO-2022-JP . iso-2022-jp)
         (RT·CharacterEncoding·EUC-KR . euc-kr)
         (RT·CharacterEncoding·CNS-11643 . chinese-cns11643)
         (RT·CharacterEncoding·Windows-1251 . windows-1251)
         (RT·CharacterEncoding·KOI8-R . koi8-r)

         (RT·CharacterEncoding·KOI8-U . koi8-u)
         (RT·CharacterEncoding·IBM866 . ibm866)
         (RT·CharacterEncoding·Mac-Cyrillic . mac-cyrillic)
         (RT·CharacterEncoding·Windows-1256 . cp1256)
         (RT·CharacterEncoding·Windows-1254 . cp1254)

         (RT·CharacterEncoding·Windows-1255 . cp1255)
         (RT·CharacterEncoding·Windows-1253 . cp1253)
         (RT·CharacterEncoding·Windows-1250 . windows-1250)
         (RT·CharacterEncoding·Windows-1257 . cp1257)
         (RT·CharacterEncoding·Windows-1258 . cp1258)

         (RT·CharacterEncoding·Windows-874 . cp874)
         (RT·CharacterEncoding·ISO-8859-2 . iso-8859-2)
         (RT·CharacterEncoding·ISO-8859-3 . iso-8859-3)
         (RT·CharacterEncoding·ISO-8859-4 . iso-8859-4)
         (RT·CharacterEncoding·ISO-8859-5 . iso-8859-5)

         (RT·CharacterEncoding·ISO-8859-6 . iso-8859-6)
         (RT·CharacterEncoding·ISO-8859-7 . iso-8859-7)
         (RT·CharacterEncoding·ISO-8859-8 . iso-8859-8)
         (RT·CharacterEncoding·ISO-8859-8-I . iso-8859-8)
         (RT·CharacterEncoding·ISO-8859-10 . iso-8859-10)

         (RT·CharacterEncoding·ISO-8859-13 . iso-8859-13)
         (RT·CharacterEncoding·ISO-8859-14 . iso-8859-14)
         (RT·CharacterEncoding·ISO-8859-15 . iso-8859-15)
         (RT·CharacterEncoding·ISO-8859-16 . iso-8859-16)
         (RT·CharacterEncoding·Mac-Roman . mac-roman)

         (RT·CharacterEncoding·CP437 . cp437)
         (RT·CharacterEncoding·CP850 . cp850)
         (RT·CharacterEncoding·UTF-32_little-endian . utf-32le)
         (RT·CharacterEncoding·UTF-32_big-endian . utf-32be)
         (RT·CharacterEncoding·UTF-7 . utf-7)

         (RT·CharacterEncoding·EBCDIC_US . ebcdic-us)
         ;; (RT·CharacterEncoding·ASCII_7-bit_with-parity . nil)
         ;; (RT·CharacterEncoding·replacement . nil)
         ;; (RT·CharacterEncoding·x-user-defined . nil)
         ;; (RT·CharacterEncoding·unknown . nil)
         )))

  

;;;-----------------------------------------------------------------------------
;;; Configuration
;;;
  
  ;; Loop/break macros
  ;;

  (defmacro RT·loop (&rest body)
    "An infinite loop construct designed to be exited via RT·break."
    `(catch 'loop-exit
       (while t
         ,@body
         )))

  (defmacro RT·break (&optional return_val)
    "Break out of an RT·loop, optionally returning RETURN_VAL."
    `(throw 'loop-exit ,return_val)
    )


  ;; Hex Conversion
  ;;

  (defun RT·bytes-to-hex (str_bytes)
    "Convert a raw byte string to a hex string."
    (mapconcat (lambda (b) (format "%02x" b)) str_bytes "")
    )


  ;; Character Set Dictionaries
  ;;

  (defvar RT·charset·name-to-idx (RT·dict·make)
    "Dictionary mapping RT character set names to authoritative integer indices."
    )

  (defvar RT·charset·emacs-to-name (RT·dict·make)
    "Dictionary mapping native Emacs coding systems to RT names."
    )

  (defvar RT·charset·name-to-emacs (RT·dict·make)
    "Dictionary mapping RT names to native Emacs coding systems."
    )

  (defvar RT·charset·idx-to-name (make-vector 256 nil)
    "Table mapping integer indices back to RT names."
    )


  ;; Generic stack interface
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

  ;; Generic dictionary interface
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

  ;; Tape Machine Core
  ;;

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

  (defun RT·TM·buffer-byte·make (buffer_target pos_byte_initial encoding_character)
    "Creates a TM that traverses BUFFER_TARGET. Bounds are checked dynamically to permit extraction."
    (let
      (
        (pos_byte_current pos_byte_initial)
        )
      (list
        (cons 'cue-leftmost (lambda () (setq pos_byte_current (with-current-buffer buffer_target (point-min)))))
        (cons 'has-right-neighbor (lambda () (< pos_byte_current (with-current-buffer buffer_target (point-max)))))
        (cons 'step (lambda () (setq pos_byte_current (1+ pos_byte_current))))
        (cons 'read (lambda () (with-current-buffer buffer_target (char-after pos_byte_current))))
        (cons 'read-type (lambda () 'byte))
        (cons 'entangled-copy (lambda () (RT·TM·buffer-byte·make buffer_target pos_byte_current encoding_character)))
        (cons 'get-pos (lambda () pos_byte_current))
        (cons 'cue (lambda (token_pos) (setq pos_byte_current token_pos)))
        (cons 'encoding (lambda () encoding_character))
        (cons 'buffer-display (lambda () buffer_target))
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

  ;; make it global
  ;;

  (or RT·TM·list_symbol-introspection (setup))
