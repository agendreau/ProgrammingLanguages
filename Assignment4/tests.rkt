#lang racket
;; Programming Languages Homework4 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and change HOMEWORK_FILE to the name of your homework file.
(require "hw4.rkt")
(require rackunit)

;; Helper functions
(define ones (lambda () (cons 1 ones)))
(define a 2)
(define test-cache (cached-assoc (list (cons 1 2) (cons 3 4) (cons 5 6)) 2))

(define tests
  (test-suite
   "Sample tests for Assignment 4"
   
   ; sequence test
   (check-equal? (sequence 0 5 1) (list 0 1 2 3 4 5) "Sequence test 1")
   (check-equal? (sequence 3 2 1) (list ) "Sequence test 2")

   ; string-append-map test
   (check-equal? (string-append-map 
                  (list "dan" "dog" "curry" "dog2") 
                  ".jpg") '("dan.jpg" "dog.jpg" "curry.jpg" "dog2.jpg") "string-append-map test 1")
   (check-equal? (string-append-map (list ) "empty") (list ) "string-append-map test 2")
   
   ; list-nth-mod test
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 2) 2 "list-nth-mod test")
   ;(check-equal? (list-nth-mod (list 0 1 2 3 4) -2) (error "list-nth-mod: negative number") "list-nth-mod test 1")
   ;(check-equal? (list-nth-mod (list ) 2) (error "list-nth-mod: empty list") "list-nth-mod test 2")
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 5) 0 "list-nth-mod test 3")
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 4) 4 "list-nth-mod test 4")
   
   ; stream-for-n-steps test
   (check-equal? (stream-for-n-steps (lambda () (cons 1 ones)) 1) (list 1) "stream-for-n-steps test 1")
   (check-equal? (stream-for-n-steps (lambda () (cons 1 ones)) 0) (list ) "stream-for-n-steps test 2")
   (check-equal? (stream-for-n-steps (lambda () (cons 1 ones)) 2) (list 1 1 ) "stream-for-n-steps test 3")
   
   ; funny-number-stream test
   (check-equal? (stream-for-n-steps funny-number-stream 16) (list 1 2 3 4 -5 6 7 8 9 -10 11 12 13 14 -15 16) "funny-number-stream test 1")
   (check-equal? (stream-for-n-steps funny-number-stream 0) (list ) "funny-number-stream test 2")
   (check-equal? (stream-for-n-steps funny-number-stream 4) (list 1 2 3 4) "funny-number-stream test 3")
   
   ; dan-then-dog test
   (check-equal? (stream-for-n-steps dan-then-dog 1) (list "dan.jpg") "dan-then-dog test 1")
   (check-equal? (stream-for-n-steps dan-then-dog 0) (list ) "dan-then-dog test 2")
   (check-equal? (stream-for-n-steps dan-then-dog 4) (list "dan.jpg" "dog.jpg" "dan.jpg" "dog.jpg") "dan-then-dog test 3")
   
   ; stream-add-zero test
   (check-equal? (stream-for-n-steps (stream-add-zero ones) 1) (list (cons 0 1)) "stream-add-zero test 1")
   (check-equal? (stream-for-n-steps (stream-add-zero ones) 0) (list ) "stream-add-zero test 2")
   (check-equal? (stream-for-n-steps (stream-add-zero ones) 3) (list (cons 0 1) (cons 0 1) (cons 0 1)) "stream-add-zero test 3")
   
   ; cycle-lists test
   (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 3) (list (cons 1 "a") (cons 2 "b") (cons 3 "a")) 
                 "cycle-lists test 1")
   (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 3) (list (cons 1 "a") (cons 2 "b") (cons 3 "a")) 
                 "cycle-lists test")
   (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a")) 3) (list (cons 1 "a") (cons 2 "a") (cons 3 "a")) 
                 "cycle-lists test 2")
   (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 0) (list ) 
                 "cycle-lists test 3")
   (check-equal? (stream-for-n-steps (cycle-lists (list 1) (list "a")) 3) (list (cons 1 "a") (cons 1 "a") (cons 1 "a")) 
                 "cycle-lists test 4")
   
   ; vector-assoc test
   (check-equal? (vector-assoc 4 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1))) (cons 4 1) "vector-assoc test 1")
   (check-equal? (vector-assoc 1 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1))) #f "vector-assoc test 2")
   (check-equal? (vector-assoc 4 (vector (cons 2 1) (cons 3 1) 4 (cons 4 1) (cons 5 1))) (cons 4 1) "vector-assoc test 3")
   (check-equal? (vector-assoc 4 (vector )) #f "vector-assoc test 4")
   
   ; cached-assoc tests
   (check-equal? (test-cache 3) (cons 3 4) "cached-assoc test 1")
   (check-equal? (test-cache 4) #f "cached-assoc test 2")
   (check-equal? (test-cache 3) (cons 3 4) "cached-assoc test 1")
   
   
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
