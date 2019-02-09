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


; function that compares the value with elements in the list and returns the element greater than value
; returns False if there is no such value
(define (find-larger num lst)
  (cond ((null? lst) #f) ; return false if list is exhaused w/o finding value
        ((and (number? (car lst))(> (car lst) num)) (car lst)) ; if prefix in list is greater than value, return prefix
        (else (find-larger num (cdr lst))) ; else recurse with suffix
  )
)

; function that returns the minimum of the numbers in L1 that is larger
; than the smallest number in L2. If there is no number in L2, L1 is used to calculate the minimum
; If there is no number in L1 larger than the smallest number in L2, the result is false (#F).
(define (min-above-min L1 L2)
  (cond ((not (eq? (minim L2) +inf.0)) (find-larger (minim L2) L1)) ; if there is minimum in L2, find larger element than the minimum in L1
        ((eq? (minim L1) +inf.0) #f) ; if there is no minimum in L1 and L2, return False
        (else (minim L1)); if there is no minimum in L2, find minimum in L1
   )
)

;(display (min-above-min '(2 a 1 3)   '(b 5 3 1))) ; check for (2 a 1 3) and (b 5 3 1)
