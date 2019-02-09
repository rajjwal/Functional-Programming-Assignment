; function that returns reveresed list if input is list else returns the input as it is
(define (reversed L)         
  (cond                                          
    ((list? L) (reverse-general L))                                            
    (else L)
  )
)


; function that takes a list L and returns reversed version of L
(define (reverse-general L)
  (cond
    ((null? L) '()) ; return L as it is if empty
    (else (append (reverse-general(cdr L)) (list (reversed(car L))))) ; recursive call to generate reveresed
   )
)

;(display(reverse-general '(1 (2 3) (4 (a (b (c d))))))) ; check for (1 (2 3) (4 (a (b (c d)))))


; the function that returns the sum of only numbers in the list L
(define (sum-up-numbers-simple  L)
  (if (null? L) ; return 0 if no list
    0
  (if (number? (car L))
    (+ (car L) (sum-up-numbers-simple  (cdr L))) ; add the element only if it is number
    (+ 0 (sum-up-numbers-simple (cdr L)))) ; add 0 if the element is not number
  )
)

;(display (sum-up-numbers-simple  '(a 100 b (200) c 300 d))) ; check for (a 100 b (200) c 300 d)


; the function that returns the sum of all the numbers (including those in nested lists) 
(define (sum-up-numbers-general L)
  (if (null? L) ; return 0 if no list
    0
  (if (number? (car L))
    (+ (car L) (sum-up-numbers-general  (cdr L))) ; add the element only if it is number
  (if (list? (car L))
    (+ (sum-up-numbers-general(car L)) (sum-up-numbers-general (cdr L))) ; if the element is list, recursively add list
    (+ 0 (sum-up-numbers-general (cdr L))))) ; if the element is 0, add 0
  )
)

;(display (sum-up-numbers-general  '(a 100 ((b ((200) c)) 300 d)))) ; check for (a 100 ((b ((200) c)) 300 d))


; function that returns minimum number from the list
; returns the maximum number possible (+inf.0) if there are no numbers
(define (minim lst)
    (cond ((null? lst) +inf.0) ; return +inf.0 if list exhausted w/o numbers
          ((and (null? (cdr lst)) (number? (car lst))) (car lst)) ; if suffix is null and prefix is number than return prefix
          ((and (number? (car lst))(< (car lst) (minim (cdr lst)))) (car lst)) ; if prefix is smaller than suffix, return prefix
          (else (minim (cdr lst))) ; else recurse with suffix
    )
)