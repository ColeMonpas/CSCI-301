#lang racket
(define (mystery L)
  (if (null? L)
      L
      (begin (displayln L) (append (mystery (cdr L))(list (car L))))
   )
)

(define (gen-list start end)
  (if (> start end)
      '()
      (append (list start) (gen-list(+ 1 start) end))
   )
  )

(define (sum L)
  (if (null? L)
      0
      (+ (car L) (sum (cdr L)))
   )
 )

(define (retrieve-first-n N L)
  (if (null? L)
      '()
      (if (> N 0)
          (append (list (car L)) (retrieve-first-n (+ -1 N)(cdr L)))
          '()
       )
  )
 )

(define (pair-sum L N)
  (if (null? L)
      #f
      (if (null? (cdr L))
          #f
          (if (equal? (+(car L) (car (cdr L))) N)
              #t
              (pair-sum (cdr L) N)
          )
      )
   )
 )

(pair-sum (gen-list 1 100) 1000)
