
  (let 
    (
      (test_ast '(1 (2 3 4 (5 6 7)) 8))
      )
    (let 
      (
        (binary_tape (RT-tree·encode test_ast 5))
        )
      (insert "\n")
      (insert "\n=== Debug Print ===\n")
      (insert (RT-tree·debug-print binary_tape 5))
      (insert "\n=== Decoded AST ===\n")
      (insert (format "%S\n" (RT-tree·decode binary_tape 5)))
      ))

=== Debug Print ===
    | 1
 10 | 2
  0 | 3
  0 | 4
111 | 5
  0 | 6
110 | 7
110 | 8

=== Decoded AST ===
(1 (2 3 4 (5 6 7)) 8)



  (let 
    (
      (test_ast '(a (b)))
      )
    (let 
      (
        (binary_tape (RT-tree·encode test_ast 5))
        )
      (insert "\n")
      (insert "\n=== Debug Print ===\n")
      (insert (RT-tree·debug-print binary_tape 5))
      (insert "\n=== Decoded AST ===\n")
      (insert (format "%S\n" (RT-tree·decode binary_tape 5)))
      ))



