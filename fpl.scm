; function that returns reveresed list if input is list else returns the input as it is
(define (reversed L)         
  (cond                                          
    ((list? L) (reverse-general L))                                            
    (else L)
  )
)


; ; function that takes a list L and returns reversed version of L
(define (reverse-general L)
  (cond
    ((null? L) '()) ; return L as it is if empty
    (else (append (reverse-general(cdr L)) (list (reversed(car L))))) ; recursive call to generate reveresed
   )
)

;(display(reverse-general '(1 (2 3) (4 (a (b (c d))))) ))