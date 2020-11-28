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
; add sublist to list of sublists
(define (add-sublist sublist List)
  (append List (list sublist))
 )
; add sublist to list of sublists
(define (add-two-sublist sublist1 sublist2 List)
  (append List (append (list sublist1)(list sublist2)))
 )
(define (add-three-sublist sublist1 sublist2 sublist3 List)
  (append List (append (list sublist1)(append (list sublist2) (list sublist3))))
 )
(define (add-four-sublist sublist1 sublist2 sublist3 sublist4 List)
  (append List (append (list sublist1)(append (list sublist2) (append (list sublist3) (list sublist4)))))
 )
; Reflexive closure
(define (Reflexive-Closure List Set)
  (if (null? Set)
      List
      (if (subset-exists? (list (car Set) (car Set)) List)
          (Reflexive-Closure List (cdr Set))
          (Reflexive-Closure (add-sublist (list (car Set) (car Set)) List) (cdr Set))
       )
   )
 )
; Symmetric closure helper function
(define (Sym-Help List E)
  (if (null? List)
      E
      (if(eqv? (caar List) (car (cdr (car List))))
         (Sym-Help (cdr List) (add-sublist (list (caar List) (car (cdr (car List)))) E))
         (if(subset-exists? (list (car (cdr (car List))) (caar List)) List)
            (Sym-Help (remove-sublist (list (car (cdr (car List))) (caar List)) (cdr List)) (add-two-sublist (list (caar List) (car (cdr (car List)))) (list (car (cdr (car List))) (caar List)) E ))
            (Sym-Help (cdr List) (add-two-sublist (list (caar List) (car (cdr (car List)))) (list (car (cdr (car List))) (caar List)) E ))
          )
       )
   )
 )
; Symmetric closure
(define (Symmetric-Closure List)
  (Sym-Help List '())
 )
; search for (x,n) if (n,y) return y (a b) (b c) (a c)
(define (find-another is L)
  (if (null? L)
      '()
      (if (eqv? (car (car L)) is)
          (car(cdr(car L)))
          (find-another is (cdr L))
       )
   )
 )
; transitive closure helper function
(define (Tran-Help List E)
   (if (null? List)
      E
      (if(eqv? (caar List) (car (cdr (car List))))
         (Tran-Help (cdr List) (add-sublist (list (caar List) (car (cdr (car List)))) E))
         (if(subset-exists? (list (car(car List)) (find-another (car(cdr(car List))) List)) List)
            (if(and (subset-exists? (list (car (cdr (car List))) (caar List)) List) (not(subset-exists? (list (car (cdr (car List))) (car (cdr (car List)))) List)))
               (Tran-Help (remove-sublist (list (car (cdr (car List))) (caar List)) (cdr List)) (add-three-sublist (list (caar List) (car (cdr (car List)))) (list (car (cdr (car List))) (caar List)) (list (car (cdr (car List))) (car (cdr (car List)))) E))
               (Tran-Help (remove-sublist (list (car (cdr (car List))) (caar List)) (remove-sublist (list (car(car List)) (find-another (car(cdr(car List))) List)) (cdr List))) (add-three-sublist (list (caar List) (car (cdr (car List)))) (list (car (cdr (car List))) (find-another (car(cdr(car List))) List)) (list (car(car List)) (find-another (car(cdr(car List))) List)) E ))
            )
            (if(subset-exists? (list (car (cdr (car List))) (caar List)) List)
               (Tran-Help (remove-sublist (list (car (cdr (car List))) (caar List)) (cdr List)) (add-four-sublist (list (caar List) (car (cdr (car List)))) (list (car (cdr (car List))) (caar List)) (list (caar List) (caar List)) (list (car (cdr (car List))) (car (cdr (car List)))) E))
               E
             )
          )
       )
   )
 )
; transitive closure
(define (Transitive-Closure List)
  (Tran-Help List '())
 )

;(Reflexive-Closure '((a a) (b b) (c c)) '(a b c))
;(Reflexive-Closure '((a a) (b b)) '(a b c))
;(Reflexive-Closure '((a a) (a b) (b b) (b c)) '(a b c))
;(Reflexive-Closure '() '(a b c))
;(Symmetric-Closure '((a a) (a b) (b a) (b c) (c b)))
;(Symmetric-Closure '((a a) (a b) (a c)))
;(Symmetric-Closure '((a a) (b b)))
;(Symmetric-Closure '())
;(Transitive-Closure '((a b) (b c) (a c)))
;(Transitive-Closure '((a a) (b b) (c c)))
;(Transitive-Closure '((a b) (b a)))
;(Transitive-Closure '((a b) (b a) (a a))) ; FIX?
;(Transitive-Closure '((a b) (b a) (a a) (b b)))
;(Transitive-Closure '())


