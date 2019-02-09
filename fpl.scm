; function that takes two simple lists as parameters,
; returns #T if the two simple lists are equal #F otherwise
(define (equalsimp lis1 lis2)
  (cond
    ((null? lis1) (null? lis2))
    ((null? lis2) #F)
    ((eq? (car lis1) (car lis2))
     (equalsimp (cdr lis1)(cdr lis2)))
    (else #F)
  )
)


; function that takes a list and returns reversed version of L
(define (reverse-general L)         
  (cond                                          
    ((null? L) L) ;return list, if empty                       
    ((null? (cdr L)) L) ;return list, if only one element                       
    (else                                        
      (cons (car (reverse-general (cdr L))) ;recursively make reverse list
            (reverse-general (cons (car L)
                       (reverse-general (cdr (reverse-general (cdr L))))))
      )
    )
  )
)


;(display (equalsimp '() (reverse-general '()))) ; check for ()
;(display (equalsimp '(c b a) (reverse-general '(a b c)))) ; check for (a b c)
;(display (equalsimp '(() b a) (reverse-general '(a b ())))) ; check for (() b c)