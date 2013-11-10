
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if(> low high) 
     null
     (cons low (sequence (+ low stride) high stride))
     )
  )

(define (string-append-map xs suffix)
  (map (lambda (x) 
         (string-append x suffix)) xs)
  )

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]
        )
  )

(define (stream-for-n-steps s n)
  (if (= n 0)
     null
     (cons (car (s)) (stream-for-n-steps (cdr(s)) (- n 1)))
     )
  )

(define funny-number-stream 
  (letrec ([f (lambda (x) 
                (if(= (remainder x 5) 0)
                   (cons (- x) (lambda () (f (+ x 1))))
                (cons x (lambda () (f (+ x 1))))))]) 
    (lambda () (f 1))))

(define dan-then-dog
    (letrec ([f (lambda (x) 
                (if(= (remainder x 2) 0)
                   (cons "dog.jpg" (lambda () (f (+ x 1))))
                (cons "dan.jpg" (lambda () (f (+ x 1))))))]) 
    (lambda () (f 1))))

(define (stream-add-zero s)
  (lambda() (cons (cons 0 (car (s))) (stream-add-zero (cdr (s)))))
  )

(define (cycle-lists xs ys)
  (letrec ([f (lambda(n)
              (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) 
                    (lambda () (f(+ n 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec([leng (vector-length vec)]
          [f (lambda(i)
               (cond [(= i leng) #f]
                     [(and (pair? (vector-ref vec i))
                           (equal? (car(vector-ref vec i)) v))
                      (vector-ref vec i)]
                     [#t (f (+ i 1))]
                     ))])
    (f 0)
    )
  )

(define (cached-assoc xs n)
  (letrec ([table (make-vector n #f)]
          [next 0]
          [f(lambda(v)
               (let ([ans (vector-assoc v table)])
                 (if ans
                     ans
                     (let ([new-ans (assoc v xs)])
                       (if new-ans
                           (begin
                             (vector-set! table next new-ans)
                             (set! next (if (= next (- n 1))
                                            0
                                            (+ next 1)))
                             new-ans)
                           #f)))))])
    f))

  
                               
                     

 