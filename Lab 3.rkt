#lang racket
; checks if x is contained in the List L
(define (member? x L) 
   (if (null? L)
      #f
      (if (equal? (car L) x)
          #t
          (member? x (cdr L))
       )
    )
 )
; checks if a list is within a given list
(define (subset? L1 L2)
   (if (null? L1)
      #t
      (if (and (member? (car L1) L2) (equal? (length L1) 1))
          #t
          (if (member? (car L1) L2)
              (subset? (cdr L1) L2)
              #f
          )
       )
    )
 )
; checks if two lists are equal are not (if they both contain eachother)
(define (set-equal? L1 L2)
  (if (and (subset? L1 L2) (subset? L2 L1))
      #t
      #f
   )
 )
; performs the union of two lists, combines both lists and removes duplicates basically 
(define (union S1 S2)
    (if (null? S2)
        S1
        (if (member? (car S2) S1)
            (union S1 (cdr S2))
            (union (cons (car S2) S1) (cdr S2))
         )
     )
 )
; performs the intersection of two lists, will only return any x that is a member of both lists. 
(define (intersect S1 S2)
    (if (null? S1)
        '()
        (if (member? (car S1) S2)
            (cons (car S1)(intersect (cdr S1) S2))
            (intersect (cdr S1) S2)
         )
     )
 )

(intersect '(1 2 3) '(2 3 4 5 6))
(union '(1 2 3) '(2 3 4 5 6))
(member? 1 '(3 2 1))
(subset? '(1 2 3) '(3 2 1))
(set-equal? '(1 2 3) '(3 2 1))
