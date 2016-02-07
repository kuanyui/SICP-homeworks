;;======================================================
;; 1-3                             [2016-02-05 金 20:13]
;;======================================================
(define (sum-biggest-two . numbers)
  (let ((sorted (sort numbers >)))
    (+ (car sorted) (car (cdr sorted)))))

;;======================================================
;; 1-4
;;======================================================

;; If b > 0, return a + b;
;; If b < 0, return a - b.

;;======================================================
;; 1-5
;;======================================================

(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))

;; (1) Applicative-order eval:
(test 0 (p))
(if (= x 0) 0 y)                        ; expand (test 0 (p))
(if (= 0 0) 0 (p))                      ; reduce
0
;; Done.

;; (2) Normal-order eval:;
(test 0 (p))
(if (= x 0) 0 y)                        ; expand (test x y)
(if (= 0 0) 0 (p))                      ; 
(if (= 0 0) 0 (p))                      ; expand (p)
(if (= 0 0) 0 (p))                      ; expand (p)
(if (= 0 0) 0 (p))                      ; expand (p)
;; (.... Cannot reduce, infinite loop....)


;;======================================================
;; Example 1-1-7: Square Roots by Newton's Method
;;======================================================

(define (my--sqrt n guessed-sqrt)
  (if (sqrt-good-enough? n guessed-sqrt)
      guessed-sqrt
      (my--sqrt n (sqrt-improve n guessed-sqrt))))

(define (sqrt-improve n guessed-sqrt)
  "return an improved guessed-sqrt"
  (avg (/ n guessed-sqrt) guessed-sqrt))

(define (sqrt-good-enough? n sqrt-n)
  (< (abs (- (abs n) (* sqrt-n sqrt-n)))
     0.001))

(define (avg . numbers)
  (/ (apply + numbers) (length numbers)))

(define (my-sqrt n)
  (my--sqrt n 1.0))

(my-sqrt 2)
;;======================================================
;; 1-6
;;======================================================

(define (new-if predicate then else)
  (cond (predicate then)
        (#t else)))

(define (new-sqrt n guessed-sqrt)
  (new-if (sqrt-good-enough? n guessed-sqrt)
      guessed-sqrt
      (new-sqrt n (sqrt-improve n guessed-sqrt))))


#|
This will result in stack overflow.
-----------------------------------------------------------
new-if is a normal function, so all args will be evaluated:

    (new-if #t A B)

Though this function will return only A, but in fact, A & B will be evaluated.

If A & B have side-effects, that is easy to understand:

    (new-if #t (display "A\n") (display "B\n"))

Both A, B will be printed.

Therefore, when using (new-sqrt 2 1.0), no matter guessed-sqrt is
already good-enough, (new-sqrt) still eval (new-sqrt) again and again
and again. 
|#

;;======================================================
;; 1-7                             [2016-02-05 金 23:13]
;;======================================================

;; (my-sqrt 0.0000001) ; => 0.03125106561775382 (wrong!)
;; (my-sqrt 1000000000000000000000000000) ; => infinite loop

;; Following is improved according the description in 1-7.

(define (my--sqrt n guessed-sqrt)
  (let ((new-guess (sqrt-improve n guessed-sqrt)))
  (if (good-enough? guessed-sqrt new-guess)
      new-guess
      (my--sqrt n new-guess))))

(define (sqrt-improve n guessed-sqrt)   ;same as above
  "return an improved guessed-sqrt"
  (avg (/ n guessed-sqrt) guessed-sqrt))

(define (good-enough? old-guess new-guess)
  (< (abs (- old-guess new-guess)) 0.001))

(define (avg . numbers)                 ;same as above
  (/ (apply + numbers) (length numbers)))

(define (my-sqrt n)                     ;same as above
  (my--sqrt n 1.0))



;; Rewrite as "block-structure" in 1-1-8:

;; Way 1
(define (ya-sqrt n)
  (define (iterator n guess)
    (let ((new-guess (improve n guess)))
      (if (good-enough? guess new-guess)
          new-guess
          (iterator n new-guess))))
  (define (improve n guess)
    "return an improved new-guess"
    (/ (+ (/ n guess) guess) 2))
  (define (good-enough? old-guess new-guess)
    (< (abs (- (abs old-guess) (abs new-guess)))
       0.001))
  (iterator n 1.0))

;; Way 2
(define (ya-sqrt n)
  (define (iterator guess)
    (let ((new-guess (improve guess)))
      (if (good-enough? guess new-guess)
          new-guess
          (iterator new-guess))))
  (define (improve guess)
    "return an improved new-guess"
    (/ (+ (/ n guess) guess) 2))
  (define (good-enough? old-guess new-guess)
    (< (abs (- (abs old-guess) (abs new-guess)))
       0.001))
  (iterator 1.0))

;;======================================================
;; 1-8                             [2016-02-06 土 19:06]
;;======================================================
(define (cubic--root n guess)
  (let ((new-guess (cubic-root-improve n guess)))
    (if (good-enough? guess new-guess)
        new-guess
        (cubic--root n new-guess))))

(define (good-enough? old-guess new-guess) ; same as above
  (< (abs (- old-guess new-guess)) 0.001))

(define (cubic-root-improve n current-guess)
  "return an improved guessed cubic root."
  (/ (+ (/ n (* current-guess current-guess))
        (* 2 current-guess))
     3))

(define (cubic-root n)
  (cubic--root n 1.0))

;; (cubic-root 125)  ; => 5.000000000287929


;;======================================================
;; 1-9
;;======================================================

#| ===== Tail-recursive fractorial=====
(define (frac n)
  (define (iterator current until product)
    (if (> current until)
        product
        (iterator (1+ current) until (* current product))))
  (iterator 1 n 1))
|#

;; 1. this is recursive:
(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9

;; 2. this is tail-recursive (iterative)
(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9

;;======================================================
;; 1-10                            [2016-02-06 土 23:07]
;;======================================================
;; (A 1 10)
(A 1 10)
(A 0 (A 1 9))
(A 0 (A 0 (A 1 8)))
(A 0 (A 0 (A 0 (A 1 7))))
(A 0 (A 0 (A 0 (A 0 (A 1 6)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
(A 0 (A 0 (A 0 (A 0 (A 0 32)))))
(A 0 (A 0 (A 0 (A 0 64))))
(A 0 (A 0 (A 0 128)))
(A 0 (A 0 256))
(A 0 512)
1024

;; (A 2 4)
(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 (A 0 (A 1 1))))
(A 1 (A 1 4))
(A 1 (A 0 (A 1 3)))
(A 1 (A 0 (A 0 (A 1 2))))
(A 1 (A 0 (A 0 (A 0 (A 1 1)))))
(A 1 (A 0 (A 0 (A 0 2))))
(A 1 (A 0 (A 0 4)))
(A 1 (A 0 8))
(A 1 16)
(A 0 (A 1 15))
(A 0 (A 0 (A 1 14)))
(A 0 (A 0 (A 0 (A 1 13))))
(A 0 (A 0 (A 0 (A 0 (A 1 12)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 11))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 10)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 9))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 8)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 7))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 6)))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 32)))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 64))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 128)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 256))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 512)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 1024))))))
(A 0 (A 0 (A 0 (A 0 (A 0 2048)))))
(A 0 (A 0 (A 0 (A 0 4096))))
(A 0 (A 0 (A 0 8192)))
(A 0 (A 0 16384))
(A 0 32768)
65536

(A 3 3) ; => 65536
;; (Too lazy to expand with only a human brain.)

f(n) = 2n
g(n) = 2^n
h(n) = 2^(2^n)


;;------------------------------------------------------
;; Fibonacci
;;------------------------------------------------------

;; Recursive
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

;; Tail-recursive
#|
(fib! 5)
(iterator 0 1 5)
(iterator 1 1 4)
(iterator 1 2 3)
(iterator 2 3 2)
(iterator 3 5 1)
(iterator 5 8 0)
|#
(define (fib! n)
  (define (iterator x y n)
    (if (= n 0)
        x
        (iterator y (+ x y) (1- n))))
  (iterator 0 1 n))

;;------------------------------------------------------
;; Count
;;------------------------------------------------------

;; Tree recursive
(define (change money)
  (define (iter coin-type-index remainder)
    ;; Two end conditions:
    (cond ((= remainder 0) 1) ;if remainder = 0, it's an available change (1)
          ((or (< remainder 0) ;if remainder < 0, it's a wrong change (0)) 
               (= coin-type-index 0))   ;...Or an invalid type coin
           0)
          ;; Still remain some money
          (else (+
                 ;; Still try to change with the current coin
                 (iter coin-type-index (- remainder (get-coin coin-type-index)))
                 ;; Try to change with another type coin
                 (iter (1- coin-type-index) remainder)))))
  (define (get-coin coin-type-index)
    (cond ((= coin-type-index 5) 50)
          ((= coin-type-index 4) 25)
          ((= coin-type-index 3) 10)
          ((= coin-type-index 2) 5)
          ((= coin-type-index 1) 1)))
  (iter 5 money))


;;======================================================
;; 1-11                            [2016-02-07 日 23:53]
;;======================================================

;; Recursive
(define (f n)
  (cond ((< n 3) n)
        ((>= n 3) (+ (f (- n 1))
                     (f (- n 2))
                     (f (- n 3))))))

(map f '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
;;      (0 1 2 3 6 11 20 37 68 125 230 423 778 1431 2632 4841 8904 16377 30122 55403 101902)

;; Iterative (Tail-recursive)
#|
(iter 0 6 (0))
(iter 1 6 (1 0))
(iter 2 6 (2 1 0))
(iter 3 6 (3 2 1 0))
(iter 4 6 (6 3 2 1 0))
(iter 5 6 (11 6 3 2 1 0))
(iter 6 6 (20 11 6 3 2 1 0))
|#
(define (g n)
  (define (iter index until recorder)
    (cond ((> index until) (car recorder))
          ((< index 3) (iter (1+ index) until (cons index recorder)))
          ((>= index 3) (iter (1+ index) until (cons (+ (list-ref recorder 0)
                                                        (list-ref recorder 1)
                                                        (list-ref recorder 2)) recorder)))))
  (iter 0 n '(0)))

(map g '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
;; (0 1 2 3 6 11 20 37 68 125 230 423 778 1431 2632 4841 8904 16377 30122 55403 101902)

;;======================================================
;; 1-12                   [2016-02-08 月 00:03 -- 00:56]
;;======================================================

;; "Write a procedure that computes elements of Pascal's triangle by
;; means of a recursive process"

;; ...I don't understand what the question means.

#| ((1)
    (1 1)
    (1 2 1)
    (1 3 3 1)
    (1 4 6 4 1))  |#

;; Iterative [FIXME] Disastrous aweful algorithm!
(define (pascal-triangle n)
  (define (iterator row-index until stack)
    (cond ((<= until 0) '())
          ((= until 1)  '(1))
          ((= row-index (1- until)) (reverse stack))
          (else (iterator (1+ row-index)
                          until
                          (cons (get-row (car stack))
                                                stack)))))
  (define (get-row previous-row)
    (append '(1) (iter-row previous-row '()) '(1)))
  (define (iter-row row result)
    (cond ((= 1 (length row) 1) (reverse result)) ;reverse can be removed
          (else (iter-row 
                      (cdr row)
                      (cons (+ (car row)
                               (cadr row)) result)))))
  (iterator 0 n '((1))))

(pascal-triangle 5)
;; => ((1) (1 1) (1 2 1) (1 3 3 1) (1 4 6 4 1))

