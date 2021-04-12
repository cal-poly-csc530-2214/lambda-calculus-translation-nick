#lang racket
 
(require rackunit)
 
#|LC	 	=	 	num
                |	 	id
                |	 	(/ id => LC)
                |	 	(LC LC)
                |	 	(+ LC LC)
                |	 	(* LC LC)
                |	 	(ifleq0 LC LC LC)
                |	 	(println LC)|#
 
(define (my-read str)
  (call-with-input-string str read))
 
;(my-read "3")
;(my-read "(+ 4 5)")
 
(define l-str
  "(+ (+ 4 5)
   (+ (+ 5 6)
      8))")
(define l
  (my-read l-str))
 
;(my-read "(/ x => (+ x 14))")
 
;; determines whether a string represents a lambda
(define (is-lam? str)
  (is-lam?/stx (my-read str)))
 
(define (is-lam?/stx stx)
  (cond
    [(number? stx) #t]
    [(symbol? stx) #t]
    [(and (list? stx)
          (= 3 (length stx))
          (equal? (first stx) '+)) #t]
    [(and (list? stx)
          (= 4 (length stx))
          (equal? (first stx) '/)
          (symbol? (second stx))
          (equal? (third stx) '=>))
     #t]
    [(and (list? stx)
          (= 3 (length stx))
          (equal? (first stx) '*)) #t]
    [(and (list? stx)
          (= 4 (length stx))
          (equal? (first stx) 'ifleq0)) #t]
    [(and (list? stx)
          (= 2 (length stx))
          (equal? (first stx) 'println)) #t]
    [(and (= 2 (length stx))
              (is-lam?/stx (first stx))
              (is-lam?/stx (second stx))) #t]
    [else #f])
 
  )
 
(check-equal? (is-lam? "(/ x => (+ x 14))") #true)
(check-equal? (is-lam? "(+ x 14)") #t)
(check-equal? (is-lam? "x") #t)
(check-equal? (is-lam? "1") #t)
(check-equal? (is-lam? "(/ x 14)") #f)
(check-equal? (is-lam? "(* x 14)") #t)
(check-equal? (is-lam? "(ifleq0 -3 1 14)") #t)
(check-equal? (is-lam? "(println 14)") #t)
(check-equal? (is-lam? "((/ x => (+ x 42)) (* 3 4))") #t)

 
;; translate prefix plus into infix plus
(define (pre-2-in str)
  (~a (pre-2-in/stx (my-read str))))

;; translate prefix plus as a list into infix plus
(define (pre-2-in/stx stx)
  (cond [(number? stx) stx]
        [(symbol? stx) stx]
        [(and (list? stx)
              (= 4 (length stx))
              (equal? (first stx) '/)
              (symbol? (second stx))
              (equal? (third stx) '=>))
         (list (second stx)
               '=
               "("
               'lambda
               'x:
               (pre-2-in/stx (fourth stx))
               ")")]
        [(and (list? stx)
              (= 3 (length stx))
              (equal? (first stx) '+))
         (list (pre-2-in/stx (second stx))
               '+
               (pre-2-in/stx (third stx)))]
        [(and (list? stx)
              (= 3 (length stx))
              (equal? (first stx) '*))
         (list (pre-2-in/stx (second stx))
               '*
               (pre-2-in/stx (third stx)))]
        [(and (list? stx)
              (= 4 (length stx))
              (equal? (first stx) 'ifleq0))
         (list (pre-2-in/stx (third stx))
               'if
               "("
               (pre-2-in/stx (second stx))
               '<=
               0
               ")"
               'else
               (pre-2-in/stx (fourth stx)))]
        [(and (list? stx)
              (= 2 (length stx))
              (equal? (first stx) 'println))
         (list 'print
               "("
               "'"
               (pre-2-in/stx (second stx))
               "'"
               ")")]
        [(and (= 2 (length stx))
              (is-lam?/stx (first stx))
              (is-lam?/stx (second stx)))
         (list (pre-2-in/stx (first stx))
               (pre-2-in/stx (second stx)))]
        [else (error 'ta "bad input: ~v\n"
                     stx)]))

(check-equal? (pre-2-in "(/ car => (+ 1 2))")  
              "(car = ( lambda x: (1 + 2) ))")
(check-equal? (pre-2-in l-str)
              "((4 + 5) + ((5 + 6) + 8))")
(check-equal? (pre-2-in "(* 4 2)")  
              "(4 * 2)")
(check-equal? (pre-2-in "(ifleq0 -2 1 2)")  
              "(1 if ( -2 <= 0 ) else 2)")
(check-equal? (pre-2-in "(println 2)")  
              "(print ( ' 2 ' ))")
(check-equal? (pre-2-in "(println (+ 1 2))")  
              "(print ( ' (1 + 2) ' ))")


;This function was written before I realized that ~a changes the list that I return into a string.
;this function takes in a list of lambda calcs and returns a string represetnation
(define (to-string lolc)
  (cond
   #;[symbol? lolc (symbol->string lolc)]
   [(list? lolc) (string-append((to-string (first lolc)) (to-string (rest lolc))))]
   [equal? '() lolc ""]
   [(number? lolc) (number->string lolc 10)]
   [(symbol? lolc) (symbol->string lolc)]
   
   [(string? lolc) #t]
  
  ))


 ;This funtion takes in the input string and outputs the python code for the lambda expression
;it currently works for all the definitions except the assignment of the id. This problem however
;compounds to the (LC LC) grammar and messes the parens up for that one though.
(define (translate str)
  (if (is-lam? str) (pre-2-in str) (error 'ta "bad input ~v" str)))

(translate l-str)
(translate "1")
(translate "(/ vroom => 3)")
(translate "(println ((/ bus => (+ x 42)) (* 3 4)))")


 