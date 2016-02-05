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
  (if (sqrt-good-enough? guessed-sqrt new-guess)
      new-guess
      (my--sqrt n new-guess))))

(define (sqrt-improve n guessed-sqrt)   ;same
  "return an improved guessed-sqrt"
  (avg (/ n guessed-sqrt) guessed-sqrt))

(define (sqrt-good-enough? old-guess new-guess)
  (< (abs (- old-guess new-guess)) 0.001))

(define (avg . numbers)                 ;same
  (/ (apply + numbers) (length numbers)))

(define (my-sqrt n)                     ;same
  (my--sqrt n 1.0))


