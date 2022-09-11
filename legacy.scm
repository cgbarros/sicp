;; Exercise 1.1

;; 10
;; ;; 10

;; (+ 5 3 4)
;; ;; 12

;; (- 9 1)
;; ;; 8

;; (/ 6 2)
;; ;; 3

;; (+ (* 2 4) (- 4 6))
;; ;; 6

;; (define a 3)
;; ;; nothing

;; (define b (+ a 1))
;; ;; nothing

;; (+ a b (* a b))
;; ;; 19

;; (= a b)
;; ;; #f

;; (if (and (> a b) (< b (* a b)))
;;     b
;;     a)

;; ;; 3

;; (cond ((= a 4) 6)
;;       ((= b 4) (+ 6 7 a))
;; 			(else 25))
;; ;; 16

;; (+ 2 (if (> b a) b a))
;; ;; 6

;; (* (cond ((> a b) a)
;;          ((< a b) b)
;; 				 (else -1))
;; 	 (+ a 1))
;; ;; 16

;; Exercise 1.2

;; (/ (+ 5
;;       4
;; 			(- 2
;; 			   (- 3
;; 				    (+ 6 (/ 4 5)))))
;; 		(* 3
;; 		   (- 6 2)
;; 			 (- 2 7)))
;; -0.24666666666666667

;; Exercise 1.3.  Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.

;; compare the numbers
;; get the 2 larger ones
;; square them
;; sum them

(define (two-larger x y z)
        (cond ((and (> x y) (> y z)) (+ (* x x) (* y y)))
				      ((and (> x y) (< y z)) (+ (* x x) (* z z)))
							((and (< x y) (> x z)) (+ (* y y) (* x x)))
							((and (< x y) (< x z)) (+ (* y y) (* z z)))))

;; Exercise 1.4

;; if b is positive it will sum a and b. If b is negative, it will subtract b from a.

;; Exercise 1.5

(define (p) (p))

(define (test x y)
  (if (= x 0)
	    0
			y))

;; (test 0 (p))

;; applicative-order:
;; it will try to evaluate 0, then (p), then test
;; since (p) evaluates as itself, it will get stuck in an infinite loop

;; normal-order
;; (if (= 0 0) 0 (p))
;; 0

;; square root by newton's method

(define (square x) (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; Exsercise 1.6

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

;; let's try (new-sqrt-itter 1.0 3)
;  (new-if (good-engough? 1.0 3) 
;           1.0
;           (sqrt-iter (improve guess 3 3)))

; (new-if #f 1.0 (sqrt-iter (improve guess 3 3)))

;; sqrt-iter will run forever. If needs to evaluate in normal-order (or maybe evaluate the predicate first)

;; Exercise 1.7

;; why good-enough? is inadequate for square roots of small numbers?

;; let's try sqrt of 0.0003
;; I changed good-enough to print the guess each time:
;; 
; (define (good-enough? guess x)
;   (begin (print guess)
;   (< (abs (- (square guess) x)) 0.001)))
;
; results:
; 1
; 0.50015
; 0.2503749100269919
; 0.12578655657676294
; 0.06408577456208668
; 0.03438350032699598
; => 0.03438350032699598
;;
;; but:
;    (* 0.03438350032699598 0.03438350032699598)
; => 0.001182225094736533
;; very far from 0.0003!
;; so the problem is that the tolerance is to big in this case. So a fixed tolerance is not very good

;; let's now try sqrt of a big number. Let's say 9876543210123456 (that's the most I can use before braking the interpreter)
;    (sqrt 9876543210123456)
; => 99380799.0012329
;    (square 99380799.0012329)
; => 9876543210123456
;; seems precise, but if the numbers truncate it might get the wrong answer at the end

; An alternative strategy for implementing good-enough? is to watch how guess changes from one iteration to the next and to stop when the change is a very small fraction of the guess. Design a square-root procedure that uses this kind of end test. Does this work better for small and large numbers?


;; (define (square x) (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      (begin (print guess "\n square of guess: ") (square guess))
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

 (define (good-enough? old-guess new-guess)
  (begin (print "old guess: " 
	              old-guess "\n" 
								"new guess: "
								new-guess "\n" 
								"subtraction: "
								(- old-guess new-guess))
   (< (abs (- old-guess new-guess)) 0.0001))) ; a bit more precision

(define (sqrt x)
  (sqrt-iter 1.0 x))

; tests:
; > (sqrt 0.0003)
; old guess: 1
; new guess: 0.50015
; subtraction: 0.49985
; old guess: 0.50015
; new guess: 0.2503749100269919
; subtraction: 0.24977508997300807
; old guess: 0.2503749100269919
; new guess: 0.12578655657676294
; subtraction: 0.12458835345022898
; old guess: 0.12578655657676294
; new guess: 0.06408577456208668
; subtraction: 0.061700782014676256
; old guess: 0.06408577456208668
; new guess: 0.03438350032699598
; subtraction: 0.029702274235090698
; old guess: 0.03438350032699598
; new guess: 0.021554307744124198
; subtraction: 0.012829192582871785
; old guess: 0.021554307744124198
; new guess: 0.017736319611954196
; subtraction: 0.003817988132170002
; old guess: 0.017736319611954196
; new guess: 0.01732538223327823
; subtraction: 0.0004109373786759657
; old guess: 0.01732538223327823
; new guess: 0.017320508761313247
; subtraction: 0.000004873471964983445
; 0.01732538223327823
;  square of guess: 
; => 0.00030016886952919297

;    (sqrt 98765432101234)
; 9938079.90016022
;  square of guess: 
; => 98765432101968.56
; old result: 9938079.900123263
; => 9938079.900123263
;    (square 9938079.900123263)
; => 98765432101234
; seems like it got worst

;; better solution from https://codology.net/post/sicp-solution-exercise-1-7/

(define (good-enough? previous-guess guess)
  (< (abs (/ (- guess previous-guess) guess)) 0.00000000001))

(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (sqrt-iter (improve guess x) x)))

;; Exercise 1.8

(define (cube x) (* x x x))

(define (cbrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      (begin (print guess "\n cube of guess: ") (cube guess))
      (cbrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (/ (+ (/ x (square guess)) 
	      (* 2 guess))
	    3))

 (define (good-enough? old-guess new-guess)
  (begin (print "old guess: " 
	              old-guess "\n" 
								"new guess: "
								new-guess "\n" 
								"subtraction: "
								(- old-guess new-guess))
   (< (abs (- old-guess new-guess)) 0.000001))) ; a bit more precision

(define (cbrt x)
  (cbrt-iter 1.0 x))

;; Section 1.2

(define (1+ x) (+ x 1))
(define (1- x) (- x 1))

(define (factorial n)
	if (= n 1)
			1
			(* n (factorial (- n 1))))

(define (factorial n)
  (define (fact-iter product counter max)
	  (if (> counter max)
		  product
			(fact-iter (* product counter) (1+ counter) max)))
	(fact-iter 1 1 n))

;; Exercise 1.9

;; (define (+ a b)
;;   (if (= a 0)
;;       b
;;       (inc (+ (dec a) b))))

; (+ 4 5)
; (inc (+ (dec 4) 5))
; (inc (+ 3 5))
; (inc (inc (+ (dec 3) 5)))
; (inc (inc (+ 2 5)))
; (inc (inc (inc (+ (dec 2) 5))))
; (inc (inc (inc (+ 1 5))))
; (inc (inc (inc (inc (+ (dec 1) 5)))))
; (inc (inc (inc (inc (inc 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9

;; (define (+ a b)
;;   (if (= a 0)
;;       b
;;       (+ (dec a) (inc b))))

; (+ 4 5)
; (+ (dec 4) (inc 5))
; (+ 3 6)
; (+ (dec 3) (inc 6))
; (+ 2 7)
; (+ (dec 2) (inc 7))
; (+ 1 8)
; (+ (dec 1) (inc 8))
; (+ 0 9)
; 9

; Exercise 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

; (A 1 10)
; (A 0 (A 1 9))
; (A 0 (A 0 (A 1 8)))
; (A 0 (A 0 (A 0 (A 1 7))))
; (A 0 (A 0 (A 0 (A 0 (A 1 6)))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 32)))))
; (A 0 (A 0 (A 0 (A 0 64))))
; (A 0 (A 0 (A 0 128)))
; (A 0 (A 0 256)
; (A 0 512)
; 1024

; (A 2 4)
; (A 1 (A 2 3))
; (A 1 (A 1 (A 2 2)))
; (A 1 (A 1 (A 1 (A 2 1))))
; (A 1 (A 1 (A 1 2)))
; (A 1 (A 1 (A 0 (A 1 1))))
; (A 1 (A 1 (A 0 2)))
; (A 1 (A 1 4))
; (A 1 (A 0 (A 1 3)))
; (A 1 (A 0 (A 0 (A 1 2))))
; (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
; (A 1 (A 0 (A 0 (A 0 2))))
; (A 1 (A 0 (A 0 4)))
; (A 1 (A 0 8))
; (A 1 16)
; (A 0 (A 1 15))
; (A 0 (A 0 (A 1 14)))
; (A 0 (A 0 (A 0 (A 1 13))))
; (A 0 (A 0 (A 0 (A 0 (A 1 12)))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 11))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 10)))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 9))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 8)))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 7))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 6)))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 32)))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 64))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 128)))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 256))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 512)))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 1024))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 2048)))))
; (A 0 (A 0 (A 0 (A 0 4096))))
; (A 0 (A 0 (A 0 8192)))
; (A 0 (A 0 16384))
; (A 0 32768)
; 65536

; (A 3 3)
; (A 2 (A 3 2))
; (A 2 (A 2 (A 3 1)))
; (A 2 (A 2 2))
; (A 2 (A 1 (A 2 1)))
; (A 2 (A 1 2))
; (A 2 (A 0 (A 1 1)))
; (A 2 (A 0 2))
; (A 2 4)
; ...
; 65536

(define (f n) (A 0 n))

; (A 0 n)
; (* 2 n)
; f(n) computes 2n

(define (g n) (A 1 n))

; (A 1 n)
; (A 0 (A 1 (- n 1)))
; ; until n = 1
; ; then it will compute 2 and double it n times
; g(n) computes 2^n

(define (h n) (A 2 n))

; (A 2 n)
; (A 1 (A 2 (- n 1)))
; ; until n = 1
; ; then it will compute (A 1 2)
; (A 1 ... (A 1 2)) ; (A 1 ... (expt 2 2)) 2^2
; (A 1 ... (A 1 4))) ; (A 1 ... (expt 2 4)) 2^2^2
; (A 1 ... (A 1 16)) ; (A 1 ... (expt 2 16)) 2^2^2^2
; computes a tower of 2^2^2... with n height

(define (fib n)
  (define (fib-iter a b counter)
	  (if (= counter n)
		     b
				 (fib-iter (+ a b) a (+ counter 1))))
(fib-iter 1 0 0))

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
	      ((or (< amount 0) (= kinds-of-coins 0)) 0)
				(else (+ (cc amount (- kinds-of-coins 1))
							   (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	      ((= kinds-of-coins 2) 5)
				((= kinds-of-coins 3) 10)
				((= kinds-of-coins 4) 25)
				((= kinds-of-coins 5) 50)))

; Exercise 1.11

; f(n) = n if n < 3
; f(n) = f(n - 1) + 2f(n -2) + 3f(n - 3) if n >= 3

; recursive:

(define (f n)
  (if (< n 3)
	    n
			(+ (f (- n 1))
			   (* 2 (f (- n 2)))
				 (* 3 (f (- n 3))))))

; iterative:

(define (iter-f n)
   (define (f-iter fn1 fn2 fn3 counter)
	    (if (= counter n)
	        fn3
	  			(f-iter fn2 fn3 (+ fn3 (* 2 fn2) (* 3 fn1)) (+ counter 1))))
  (if (< n 3)
	   n
     (f-iter 0 1 2 2)))

; Exercise 1.12

(define (pasc x y)
  (cond ((> x y) (print "not valid"))
	      ((or (= x 0) (= x y)) 1)
				(else (+ (pasc x (- y 1)) (pasc (- x 1) (- y 1))))))

; Exercise 1.13 -> pulei (prova matemática)

; Exercise 1.14 (ver folha)

; Exercise 1.15

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

; a)

; (sine 12.15)

; (p (sine (/ 12.15 3.0)))
; (p (sine 4.05))
; (p (p (sine (/ 4.05 3.0))))
; (p (p (sine 1.3499999999999999)))
; (p (p (p (sine (/ 1.3499999999999999 3.0)))))
; (p (p (p (sine 0.44999999999999996))))
; (p (p (p (p (sine (/ 0.44999999999999996 3.0)))))
; (p (p (p (p (sine 0.15)))))
; (p (p (p (p (p (sine (/ 0.15 3.0)))))))
; (p (p (p (p (p (sine 0.05)))))))
; now it begins to shrink
; p is applied 5 times

; b)

; at every step the angle is divided by 3, therefore the every 3N steps increases in 1

; this is O(log(n))

; Exponentiation

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                (- counter 1)
                (* b product)))) 

define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (mod n 2) 0))


; Exercise 1.16

(define (square x) (* x x))
(define (1- x) (- x 1))
(define (1+ x) (+ x 1))

(define (fast-expt b n)
  (define (iter a exponent)
	  (cond ((= exponent 1) a)
		      ((even? exponent) (iter (square a) (/ exponent 2)))
					(else (iter (* a b) (1- exponent)))))
	(iter b n))

; Exercise 1.17

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (fast-mult a b)
  (cond ((or (= a 0) (= b 0)) 0)
	      ((= b 1) a)
	      ((even? b) (fast-mult (double a) (halve b)))
				(else (+ a (fast-mult a (- b 1))))))

; Exercise 1.18

; my solution

; (define (fast-mult a b)
;   (define (iter result remainder)
; 	  (cond ((= remainder 0) result)
; 		      ((even? remainder) (iter (+ result (double a)) (- remainder 2)))
; 					(else (iter (+ result a) (- remainder 1)))))
; 	(iter 0 b))

; better solution

	(define (fast-mult-iter a b)
    (define (iter multiplier multiplicand result)
		  (cond ((= multiplicand 0) result)
			      ((even? multiplicand) (iter (double multiplier) (halve multiplicand) result))
						(else (iter multiplier (- multiplicand 1) (+ result multiplier)))))
		(iter a b 0))

; Exercise 1.19

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square q) (square p))bhn
                   (+ (square q) (* 2 p q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

; Exercise 1.20

;; (gcd 206 40)
;; (if (= 40 0) 206 (gcd 40 (remainder 206 40)))
;; (gcd 40 (remainder 206 40))
;; (if (= (remainder 206 40) 0) 40 (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
;; (if (= 6 0) 40 (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))) ;1
;; (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
;; (if (= (remainder 40 (remainder 206 40)) 0) (remainder 206 40) (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
;; (if (= (remainder 40 6) 0) (remainder 206 40) (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
;; (if (= 4 0) (remainder 206 40) (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))) ;2
;; (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
;; (if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0) (remainder 40 (remainder 206 40)) (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
;; (if (= 2 0) (remainder 40 (remainder 206 40)) (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))) ;4
;; (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
;; (if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))))

; (if (= 0 0) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))) ;7

; (remainder (remainder 206 40) (remainder 40 6))

2 ; 3

; reminder is calculated 17 times

; (gcd 206 40)
; (if (= 40 0) 206 (gcd 40 (remainder 206 40)))
; (if #f 206 (gcd 40 6)) ; 1
; (gcd 40 6)
; (if (= 6 0) 40 (gcd 6 (remainder 40 6)))
; (if #f 40 (gcd 6 4)) ; 2
; (gcd 6 4)
; (if (= 4 0) 6 (gcd 4 (remainder 6 4)))
; (if #f 6 (gcd 4 2)) ; 3
; (gcd 4 2)
; (if (= 2 0) 4 (gcd 2 (remainder 4 2)))
; (if #f 4 (gcd 2 0)) ; 4
; (gcd 2 0)
; (if (= 0 0) 2 (gcd 0 (remainder 2 0)))
; 2

; reminder is calculated 4 times

; Exercise 1.21

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (mod b a) 0))

; > (smallest-divisor 199)
; => 199

; > (smallest-divisor 1999)
; => 1999

; > (smallest-divisor 19999)
; => 7

; Exercise 1.22

(define (1+ x) (+ x 1))
(define (1- x) (- x 1))
(define inc 1+)
(define (square x) (* x x))

(define (remainder a b) (mod a b))

(define (date2runtime date)
  ; HACK
  ; wraps around occasionally!
  (+  
     (* (date-hour date) 60 60 1000) 
     (* (date-minute date) 60 1000) 
     (* (date-second date) 1000) 
     (date-millisecond date)
  )
)

(define (runtime) (date2runtime (current-date)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

; prime test

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (search-for-primes range-start range-end)
  (cond ((even? range-start)
	         (search-for-primes (1+ range-start) range-end))
				((< range-start (+ range-end 1))
				   (timed-prime-test range-start) (search-for-primes (+ 2 range-start) range-end))))

  ; (search-for-primes 1000 1100)
; 1001
; 1003
; 1005
; 1007
; 1009 *** 2
; 1011
; 1013 *** 2
; 1015
; 1017
; 1019 *** 2

;   (search-for-primes 10000 10100)

; 10001
; 10003
; 10005
; 10007 *** 6
; 10009 *** 7
; 10011
; 10013
; 10015
; 10017
; 10019
; 10021
; 10023
; 10025
; 10027
; 10029
; 10031
; 10033
; 10035
; 10037 *** 7

;; 2*sqrt(10) = 6.32455532034 which seems correct

;   (search-for-primes 100000 100100)

; 100001
; 100003 *** 21
; 100005
; 100007
; 100009
; 100011
; 100013
; 100015
; 100017
; 100019 *** 20
; 100021
; 100023
; 100025
; 100027
; 100029
; 100031
; 100033
; 100035
; 100037
; 100039
; 100041
; 100043 *** 19

; 7*sqrt(10) = 22.1359436212 close enough again, although it seems like for bigger number it's getting a little better

; Exercise 1.23

(define (next x) 
  (if (= x 2)
	    3
			(+ x 2)))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (mod b a) 0))

(define (1+ x) (+ x 1))
(define (1- x) (- x 1))
(define inc 1+)
(define (square x) (* x x))

(define (remainder a b) (mod a b))

(define (date2runtime date)
  ; HACK
  ; wraps around occasionally!
  (+  
     (* (date-hour date) 60 60 1000) 
     (* (date-minute date) 60 1000) 
     (* (date-second date) 1000) 
     (date-millisecond date)
  )
)

(define (runtime) (date2runtime (current-date)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (search-for-primes range-start range-end)
  (cond ((even? range-start)
	         (search-for-primes (1+ range-start) range-end))
				((<= range-start range-end)
				   (timed-prime-test range-start) (search-for-primes (+ 2 range-start) range-end))))

; (timed-prime-test 1009)
; 1009 *** 1   

; (timed-prime-test 1013)
; 1013 *** 2   

; (timed-prime-test 1019)
; 1019 *** 2 

;; first one was quicker, the rest is the same

; (timed-prime-test 10007)
; 10007 *** 4   

; (timed-prime-test 10009)
; 10009 *** 5   

; (timed-prime-test 10037)
; 10037 *** 5   

;; before: 6,7,7

; (timed-prime-test 100003)
; 100003 *** 12   

; (timed-prime-test 100019)
; 100019 *** 13   

; (timed-prime-test 100043)
; 100043 *** 15   

;; before: 21,20,19

;; it's deffinitely quicker, but not half of speed. I think in part is CPU using other stuff and in part imprecision of the test itself

;; better explanation at https://codology.net/post/sicp-solution-exercise-1-23/

; The improved algorithm did cut down the number of steps by 2, but the reason that it didn’t speedup the computation by 200% was because we added some new work for each steps:

; one more function call
; an if
; the predicate (= n 2)
; Calendar

; Exercise 1.24

(define (1+ x) (+ x 1))
(define (1- x) (- x 1))
(define inc 1+)
(define (square x) (* x x))

(define (remainder a b) (mod a b))

(define (date2runtime date)
  ; HACK
  ; wraps around occasionally!
  (+  
     (* (date-hour date) 60 60 1000) 
     (* (date-minute date) 60 1000) 
     (* (date-second date) 1000) 
     (date-millisecond date)
  )
)

(define (runtime) (date2runtime (current-date)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (fast-prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

; prime test

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (search-for-primes range-start range-end)
  (cond ((even? range-start)
	         (search-for-primes (1+ range-start) range-end))
				((<= range-start range-end)
				   (timed-prime-test range-start) (search-for-primes (+ 2 range-start) range-end))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; 1000003 *** 64
; 1000033 *** 60
; 1000037 *** 61

; seems like it still increased by about 3 times.

; Exercise 1.25

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;; original expmod

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(expmod 2 4 3)
(remainder (square (expmod 2 2 3)) 3)
(remainder (square (remainder (square (expmod 2 1 3)) 3)) 3)
(remainder (square (remainder (square (remainder (* 2 (expmod 2 0 3)) 3)) 3)) 3)
(remainder (square (remainder (square (remainder (* 2 1) 3)) 3)) 3)
(remainder (square (remainder (square (remainder 2 3)) 3)) 3)
(remainder (square (remainder (square 2) 3)) 3)
(remainder (square (remainder 4 3)) 3)
(remainder (square 1) 3)
(remainder 1 3)
1

;; Alyssa's expmod

(define (expmod-alyssa base exp m)
  (remainder (fast-expt base exp) m))


(expmod 2 4 3)

(remainder (fast-expt 2 4) 3)
(remainder (square (fast-expt 2 2)) 3)
(remainder (square (square (fast-expt 2 1))) 3)
(remainder (square (square (* 2 (fast-expt 2 0)))) 3)
(remainder (square (square (* 2 1))) 3)
(remainder (square (square 2)) 3)
(remainder (square 4) 3)
(remainder 16 3)
1

;; we do more remainder operations in the original expmod, but so far seems like the results are the same

; best answer - https://codology.net/post/sicp-solution-exercise-1-25/
;; alyssa's implementation square the bases many times, which for large numbers can run out of memory. The original expmod calculates the remainder first, which keeps the numbers low.

; Exercise 1.26

Louis Reasoner's code:

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

; By writing the explicit multiplication, the code is computing expmod 2 times, which makes the process slower

; Exercise 1.27

(define (square x) (* x x))

(define random random-integer)

(define remainder mod)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))  

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (fermat-test-all n)
	(define (all-tries a)
	  (cond ((= a 0) #t)
		      ((= (expmod a n n) a) (all-tries (- a 1)))
					(else #f)))
  (all-tries (- n 1)))

;    (fermat-test-all 561)
; => #t
;    (fermat-test-all 1105)
; => #t
;    (fermat-test-all 1729)
; => #t
;    (fermat-test-all 2465)
; => #t
;    (fermat-test-all 2821)
; => #t
;    (fermat-test-all 6601)
; => #t

; Exercise 1.28

(define nil '())
(define true #t)
(define false #f)
(define (square x) (* x x))

(define remainder mod)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	      ((and (not (= base 1)) (not (= base (- m 1))) (= (remainder (square base) m) 1))
				 0)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m)))) 

(define (miller-rabin n)
  (define (try-it a)
	 (= (expmod a (- n 1) n) 1))
(try-it (+ 1 (random (- n 1)))))

(define (fast-miller-rabin? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))


(define (miller-rabin-all n)
	(define (all-tries a)
	  (cond ((= a 0) #t)
		      ((= (expmod a (- n 1) n) 1) (all-tries (- a 1)))
					(else #f)))
  (all-tries (- n 1)))

; (remainder (square (expmod base (/ exp 2) m)) m))

;    (miller-rabin-all 561)
; => #f
;    (miller-rabin-all 1105)
; => #f
;    (miller-rabin-all 1729)
; => #f
;    (miller-rabin-all 2465)
; => #f
;    (miller-rabin-all 2821)
; => #f
;    (miller-rabin-all 6601)
; => #f

;    (fast-miller-rabin? 1 100)
; => #f
;    (fast-miller-rabin? 2 100)
; => #t
;    (fast-miller-rabin? 3 100)
; => #t
;    (fast-miller-rabin? 4 100)
; => #f
;    (fast-miller-rabin? 5 100)
; => #t
;    (fast-miller-rabin? 6 100)
; => #f
;    (fast-miller-rabin? 7 100)
; => #t
;    (fast-miller-rabin? 8 100)
; => #f
;    (fast-miller-rabin? 9 100)
; => #f
;    (fast-miller-rabin? 10 100)
; => #f
;    (fast-miller-rabin? 11 100)
; => #t
;    (fast-miller-rabin? 12 100)
; => #f
;    (fast-miller-rabin? 13 100)
; => #t