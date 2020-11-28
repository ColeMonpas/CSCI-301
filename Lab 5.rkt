#lang racket
; checks if x is contained in the List L
(define (member? x L) 
   (if (null? L)
      #f
      (if (eqv? (car L) x)
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
(define (list-equal? L1 L2)
  (if (and (subset? L1 L2) (subset? L2 L1))
      #t
      #f
   )
 )
; checks if a given set exists within another set. 
(define (subset-exists? Sub List)
  (if (null? List)
      #f
      (if (list? (car List))
          (if (equal? Sub (car List)) ;HERE
              #t
              (subset-exists? Sub (cdr List))
           )
          (subset-exists? Sub (cdr List))
       )
  )
 )
; remove an item from a list
(define (remove x list)
  (if (null? list)
      '()
      (if (eqv? x (car list))
          (remove x (cdr list))
          (cons (car list)(remove x (cdr list)))
       )
   )
)
; remove a sublist from a list
(define (remove-sublist sub list)
  (if (null? list)
      '()
      (if (equal? sub (car list))
          (remove sub (cdr list))
          (cons (car list)(remove-sublist sub (cdr list)))
       )
   )
)
; checks to find all sub reflecive lists in a list (is a helper dont use alone)
(define (contains-all-reflex L S)
  (if (null? S)
      #t
      (if (subset-exists? (list (car S) (car S)) L)
          (contains-all-reflex L (cdr S))
          #f
       )
   )
 )
; relexive determines if L has a reflective relation over S
(define (Reflexive? L S)
  (if (null? L)
          #t
       (if (and (member? (car(car L)) S) (equal? (car(car L)) (car(cdr(car L)))))
           (if (contains-all-reflex L S)
               (Reflexive? (cdr L) (remove (car(car L)) S))
               #f
            )
            #f
       )
   )
 )
; Symmetric determines if L has a symmetric relation 
(define (Symmetric? L)
  (if (null? L)
      #t
      (if (equal? (car(car L)) (car(cdr(car L))))
          (Symmetric? (cdr L))
          (if(subset-exists? (list (car(cdr(car L))) (car(car L))) L)
             (Symmetric? (remove-sublist (list (car(cdr(car L))) (car(car L))) (cdr L)))
             #f
           )
       )
   )
 )
; another not equal but not
(define (find-another is L)
  (if (null? L)
      '()
      (if (eqv? (car (car L)) is)
          (car(cdr(car L)))
          (find-another is (cdr L))
       )
   )
 )
; transitive
(define (Transitive? L)
  (if (null? L)
      #f
      (if (subset-exists? (list (car(car L)) (find-another (car(cdr(car L))) L)) L)
          #t
          (Transitive? (cdr L))
       )
   )
 )

(Transitive? '((a b) (b c) (a c)))
(Transitive? '((a a) (b b) (c c)))
(Transitive? '((a b) (b a)))
(Transitive? '((a b) (b a) (a a)))
(Transitive? '((a b) (b a) (a a) (b b)))
;(Reflexive? '((a a) (b b) (c c)) '(a b c))
;(Reflexive? '((a a) (b b)) '(a b c))
;(Reflexive? '((a a) (a s) (b b) (c c)) '(a b c))
;(Reflexive? '() '())
;(Symmetric? '((a a) (a b) (b a) (b c) (c b)))
;(Symmetric? '((a a) (a b) (a c) (c a)))
;(Symmetric? '((a a) (b b)))
;(Symmetric? '())
