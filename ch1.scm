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


