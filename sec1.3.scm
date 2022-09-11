(define (square a) (* a a))
(define (cube a) (* a a a))
(define nil '())
(define (average x y) (/ (+ x y) 2))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

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

; 1.25  Alyssa P. Hacker complains that we went to a lot of extra work in writing expmod. After all, she says, since we already know how to compute exponentials, we could have simply written

(define (fast-expt b n) 
   (cond ((= n 0) 1) 
         ((even? n) (square (fast-expt b (/ n 2)))) 
         (else (* b (fast-expt b (- n 1))))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square ( base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (expmod2 base exp m)
  (remainder (fast-expt base exp) m))

; Is she correct? Would this procedure serve as well for our fast prime tester? Explain.

; => best answer - https://codology.net/post/sicp-solution-exercise-1-25/
; => alyssa's implementation square the bases many times, which for large numbers can run out of memory. The original expmod calculates the remainder first, which keeps the numbers low.

; Exercise 1.26.  Louis Reasoner is having great difficulty doing exercise 1.24. His fast-prime? test seems to run more slowly than his prime? test. Louis calls his friend Eva Lu Ator over to help. When they examine Louis's code, they find that he has rewritten the expmod procedure to use an explicit multiplication, rather than calling square:

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;``I don't see what difference that could make,'' says Louis. ``I do.'' says Eva. ``By writing the procedure like that, you have transformed the (log n) process into a (n) process.'' Explain.

; By writing the explicit multiplication, the code is computing expmod 2 times, which increse the steps exponentially

; Exercise 1.27.  Demonstrate that the Carmichael numbers listed in footnote 47 really do fool the Fermat test. That is, write a procedure that takes an integer n and tests whether an is congruent to a modulo n for every a<n, and try your procedure on the given Carmichael numbers.

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

; Exercise 1.28.  One variant of the Fermat test that cannot be fooled is called the Miller-Rabin test (Miller 1976; Rabin 1980). This starts from an alternate form of Fermat's Little Theorem, which states that if n is a prime number and a is any positive integer less than n, then a raised to the (n - 1)st power is congruent to 1 modulo n. To test the primality of a number n by the Miller-Rabin test, we pick a random number a<n and raise a to the (n - 1)st power modulo n using the expmod procedure. However, whenever we perform the squaring step in expmod, we check to see if we have discovered a ``nontrivial square root of 1 modulo n,'' that is, a number not equal to 1 or n - 1 whose square is equal to 1 modulo n. It is possible to prove that if such a nontrivial square root of 1 exists, then n is not prime. It is also possible to prove that if n is an odd number that is not prime, then, for at least half the numbers a<n, computing an-1 in this way will reveal a nontrivial square root of 1 modulo n. (This is why the Miller-Rabin test cannot be fooled.) Modify the expmod procedure to signal if it discovers a nontrivial square root of 1, and use this to implement the Miller-Rabin test with a procedure analogous to fermat-test. Check your procedure by testing various known primes and non-primes. Hint: One convenient way to make expmod signal is to have it return 0.

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	      ((and (not (= base 1)) 
				      (not (= base (- m 1))) 
							(= (remainder (square base) m) 1))
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

(define (miller-rabin-all n)
	(define (all-tries a)
	  (cond ((= a 0) #t)
		      ((= (expmod a (- n 1) n) 1) (all-tries (- a 1)))
					(else #f)))
  (all-tries (- n 1)))

	;Exercise 1.13.  Prove that Fib(n) is the closest integer to n/5, where  = (1 + 5)/2. Hint: Let  = (1 - 5)/2. Use induction and the definition of the Fibonacci numbers (see section 1.2.2) to prove that Fib(n) = (n - n)/5.

;;;;;;; Section 1.3

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

; Exercise 1.29.  Simpson's Rule is a more accurate method of numerical integration than the method illustrated above. Using Simpson's Rule, the integral of a function f between a and b is approximated as

; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/ch1-Z-G-29.gif

; where h = (b - a)/n, for some even integer n, and yk = f(a + kh). (Increasing n increases the accuracy of the approximation.) Define a procedure that takes as arguments f, a, b, and n and returns the value of the integral, computed using Simpson's Rule. Use your procedure to integrate cube between 0 and 1 (with n = 100 and n = 1000), and compare the results to those of the integral procedure shown above.

(define (simpson-integral f a b n)
  (define (y k) (f (+ a (* k h))))
	(define h (/ (- b a) n))
  (define (simpson-term k) 
    (cond ((or (= k 0) (= k n)) (y k))
		      ((odd? k) (* 4 (y k)))
					((even? k) (* 2 (y k)))))
	(* (/ h 3)
	   (sum simpson-term 0.0 inc n)))

; 1 ]=> (simpson-integral cube 0 1 100)

; ;Value: .25333333333333324

; 1 ]=> (simpson-integral cube 0 1 1000)

; ;Value: .2503333333333336

; Exercise 1.30.  The sum procedure above generates a linear recursion. The procedure can be rewritten so that the sum is performed iteratively. Show how to do this by filling in the missing expressions in the following definition:

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

; Exercise 1.31.  
; a.  The sum procedure is only the simplest of a vast number of similar abstractions that can be captured as higher-order procedures. Write an analogous procedure called product that returns the product of the values of a function at points over a given range. Show how to define factorial in terms of product. Also use product to compute approximations to  using the formula

; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/ch1-Z-G-30.gif


(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

	(define (factorial n)
	  (product identity 1 inc n))
	
	(define (pi n)
	  (define (numerator-term a)
		  (cond ((odd? a) (+ a 3))
						((even? a) (+ a 2))))
		(define (denominator-term a)
		  (cond ((odd? a) (+ a 2))
			      ((even? a) (+ a 3))))
		(* 4.0 (/ (product numerator-term 0.0 inc n)
		        (product denominator-term 0.0 inc n))))

; b.  If your product procedure generates a recursive process, write one that generates an iterative process. If it generates an iterative process, write one that generates a recursive process.

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

; Exercise 1.32.  a. Show that sum and product (exercise 1.31) are both special cases of a still more general notion called accumulate that combines a collection of terms, using some general accumulation function:

; (accumulate combiner null-value term a next b)

; Accumulate takes as arguments the same term and range specifications as sum and product, together with a combiner procedure (of two arguments) that specifies how the current term is to be combined with the accumulation of the preceding terms and a null-value that specifies what base value to use when the terms run out. Write accumulate and show how sum and product can both be defined as simple calls to accumulate.

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate combiner null-value term (next a) next b))))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (sum term a next b)
  (accumulate + 0 term a next b))

; b. If your accumulate procedure generates a recursive process, write one that generates an iterative process. If it generates an iterative process, write one that generates a recursive process.

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

; Exercise 1.33.  You can obtain an even more general version of accumulate (exercise 1.32) by introducing the notion of a filter on the terms to be combined. That is, combine only those terms derived from values in the range that satisfy a specified condition. The resulting filtered-accumulate abstraction takes the same arguments as accumulate, together with an additional predicate of one argument that specifies the filter. Write filtered-accumulate as a procedure. Show how to express the following using filtered-accumulate:

(define (accumulate combiner filter? null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
		      ((filter? (term a)) (iter (next a) (combiner result (term a))))
          (else (iter (next a) result))))
  (iter a null-value))

(define (null-filter a) #t)

(define (product term a next b)
  (accumulate * null-filter 1 term a next b))

(define (sum term a next b)
  (accumulate + null-filter 0 term a next b))

; a. the sum of the squares of the prime numbers in the interval a to b (assuming that you have a prime? predicate already written)

(define (sum-of-squares-of-primes a b)
  (accumulate + prime? 0 square a inc b))

; b. the product of all the positive integers less than n that are relatively prime to n (i.e., all positive integers i < n such that GCD(i,n) = 1).

(define (product-of-relative-primes a n)
  (define (relative-prime? i)
    (= (gcd i n) 1))
	(accumulate * relative-prime? 1 identity a inc n))

; Exercise 1.34.  Suppose we define the procedure

(define (f g)
  (g 2))

; Then we have

; (f square)
; 4

; (f (lambda (z) (* z (+ z 1))))
; 6

; What happens if we (perversely) ask the interpreter to evaluate the combination (f f)? Explain.

; (f f)
; (f 2)
; (2 2)
; error

; real try:
;; 1 ]=> (f f)
;; ;The object 2 is not applicable.

; Finding roots of equations by the half-interval method

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

; Finding fixed points of functions

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point (lambda (y) (/ x y))
               1.0))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

; Exercise 1.35.  Show that the golden ratio  (section 1.2.2) is a fixed point of the transformation x => 1 + 1/x, and use this fact to compute  by means of the fixed-point procedure.

; phi^2 = phi + 1
; phi = (phi + 1) / phi
; = phi/phi + 1/phi
; = 1 + 1/phi

(define phi (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

; Exercise 1.36.  Modify fixed-point so that it prints the sequence of approximations it generates, using the newline and display primitives shown in exercise 1.22. Then find a solution to x^x = 1000 by finding a fixed point of x => log(1000)/log(x). (Use Scheme's primitive log procedure, which computes natural logarithms.) Compare the number of steps this takes with and without average damping. (Note that you cannot start fixed-point with a guess of 1, as this would cause division by log(1) = 0.)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (begin (newline) (display guess) (try next)))))
  (try first-guess))

;; (fixed-point (lambda (x) (/ (log 1000) (log x))) 10)

; Exercise 1.37.  

; a. An infinite continued fraction is an expression of the form

; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/ch1-Z-G-34.gif

; As an example, one can show that the infinite continued fraction expansion with the Ni and the Di all equal to 1 produces 1/phi, where phi is the golden ratio (described in section 1.2.2). One way to approximate an infinite continued fraction is to truncate the expansion after a given number of terms. Such a truncation -- a so-called k-term finite continued fraction -- has the form

; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/ch1-Z-G-35.gif

; Suppose that n and d are procedures of one argument (the term index i) that return the Ni and Di of the terms of the continued fraction. Define a procedure cont-frac such that evaluating (cont-frac n d k) computes the value of the k-term finite continued fraction. Check your procedure by approximating 1/phi using

; (cont-frac (lambda (i) 1.0)
;            (lambda (i) 1.0)
;            k)

; for successive values of k. How large must you make k in order to get an approximation that is accurate to 4 decimal places?

(define (cont-frac n d k)
  (define (rec i)
	        (if (= i k)
					    (/ (n i) (d i))
							(/ (n i) (+ (d i) (rec (inc i))))))
  (rec 0))

	

; ]=> (/ 1 phi)

;Value: .6180344478216819
	
; ]=> (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10)

;Value: .6180555555555556

; b. If your cont-frac procedure generates a recursive process, write one that generates an iterative process. If it generates an iterative process, write one that generates a recursive process.

(define (cont-frac n d k)
  (define (iter result i)
	        (if (= i 0) 
							result
							(iter (/ (n i) (+ (d i) result)) (- i 1))))
  (iter (/ (n k) (d k)) (- k 1)))

;; Exercise 1.38.  In 1737, the Swiss mathematician Leonhard Euler published a memoir De Fractionibus Continuis, which included a continued fraction expansion for e - 2, where e is the base of the natural logarithms. In this fraction, the Ni are all 1, and the Di are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, .... Write a program that uses your cont-frac procedure from exercise 1.37 to approximate e, based on Euler's expansion.

(define (e precision)
	(define (denominator i)
		(if (= (remainder (- i 1) 3) 0)
				(* (+ (quotient i 3) 1) 2)
				1))
	(+ 2 (cont-frac (lambda (i) 1.0) denominator precision)))

;; Exercise 1.39.  A continued fraction representation of the tangent function was published in 1770 by the German mathematician J.H. Lambert:

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/ch1-Z-G-36.gif

;; where x is in radians. Define a procedure (tan-cf x k) that computes an approximation to the tangent function based on Lambert's formula. K specifies the number of terms to compute, as in exercise 1.37.

(define (tan-cf x k)
	(define (numerator i)
		(if (= i 1)
				x
		    (- (square x))))
	(cont-frac numerator 
						 (lambda (i) (- (* i 2.0) 1))
						 k))

;; 1.3.4

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

;(sqrt 2)
;Value: 1.4142135623822438

;; Abstractions and first-class procedures

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

;(sqrt 2)
;Value: 1.4142135623746899

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))

;(sqrt 2)
;Value: 1.4142135623822438

;; Exercise 1.40.  Define a procedure cubic that can be used together with the newtons-method procedure in expressions of the form

;; (newtons-method (cubic a b c) 1)

;; to approximate zeros of the cubic x^3 + ax^2 + bx + c.

(define (cubic a b c) 
	(lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))


;; Exercise 1.41.  Define a procedure double that takes a procedure of one argument as argument and returns a procedure that applies the original procedure twice. For example, if inc is a procedure that adds 1 to its argument, then (double inc) should be a procedure that adds 2. What value is returned by

;; (((double (double double)) inc) 5)

(define (double proc)
	(lambda (x) (proc (proc x))))

;; 1 ]=> (((double (double double)) inc) 5)
;Value: 21


;; 1 ]=> (((double (double double)) inc) 1)

;; ;Value: 17


;; Exercise 1.42.  Let f and g be two one-argument functions. The composition f after g is defined to be the function x => f(g(x)). Define a procedure compose that implements composition. For example, if inc is a procedure that adds 1 to its argument,

;; ((compose square inc) 6)
;; 49

(define (compose f g)
	(lambda (x) (f (g x))))


;; Exercise 1.43.  If f is a numerical function and n is a positive integer, then we can form the nth repeated application of f, which is defined to be the function whose value at x is f(f(...(f(x))...)). For example, if f is the function x => x + 1, then the nth repeated application of f is the function x => x + n. If f is the operation of squaring a number, then the nth repeated application of f is the function that raises its argument to the 2nth power. Write a procedure that takes as inputs a procedure that computes f and a positive integer n and returns the procedure that computes the nth repeated application of f. Your procedure should be able to be used as follows:

;; ((repeated square 2) 5)
;; 625

;; Hint: You may find it convenient to use compose from exercise 1.42.

(define (repeated f n)
	(if (= n 1)
			(lambda (x) (f x))
			(compose f (repeated f (- n 1)))))

(define (repeated f n)
	(lambda (x)
		(define (iter i result)
			(if (= i 1)
					result
					(iter (- i 1) (f result))))
		(iter n (f x))))

;; Exercise 1.44.  The idea of smoothing a function is an important concept in signal processing. If f is a function and dx is some small number, then the smoothed version of f is the function whose value at a point x is the average of f(x - dx), f(x), and f(x + dx). Write a procedure smooth that takes as input a procedure that computes f and returns a procedure that computes the smoothed f. It is sometimes valuable to repeatedly smooth a function (that is, smooth the smoothed function, and so on) to obtained the n-fold smoothed function. Show how to generate the n-fold smoothed function of any given function using smooth and repeated from exercise 1.43.

(define (smooth f)
	(lambda (x) 
		(/ 
			 (+ (f (- x dx)) ; dx is already defined
					(f x) 
					(f (+ x dx))) 
		   3)))


;; 1 ]=> (((repeated smooth 10) sin) 2)

;; ;Value: .9092974265225826

;; Exercise 1.45.  We saw in section 1.3.3 that attempting to compute square roots by naively finding a fixed point of y => x/y does not converge, and that this can be fixed by average damping. The same method works for finding cube roots as fixed points of the average-damped y => x/y2. Unfortunately, the process does not work for fourth roots -- a single average damp is not enough to make a fixed-point search for y => x/y3 converge. On the other hand, if we average damp twice (i.e., use the average damp of the average damp of y => x/y3) the fixed-point search does converge. Do some experiments to determine how many average damps are required to compute nth roots as a fixed-point search based upon repeated average damping of y  x/y^n-1. Use this to implement a simple procedure for computing nth roots using fixed-point, average-damp, and the repeated procedure of exercise 1.43. Assume that any arithmetic operations you need are available as primitives.

;; (define (cube-root x)
;;   (fixed-point (average-damp (lambda (y) (/ x (square y))))
;;                1.0))

(define (fourth-root x)
	(fixed-point ((repeated average-damp 2) 
								(lambda (y) (/ x (cube y)))) 
							 1.0))

;;; for testing:
;; (define (nth-root x times n)
;; 	(fixed-point ((repeated average-damp times) 
;; 								(lambda (y) (/ x (expt y (- n 1)))))
;; 							 1.0))

; average dumping twice works untill 2^7
; (nth-root 256 2 8) doesn't converge
; (nth-root 256 3 16) doesn't converge
; so it has to be < than (expt 2 (+ n 1))

(define (nth-root x n)
	(fixed-point ((repeated average-damp (round (sqrt n)))
								(lambda (y) (/ x (expt y (- n 1)))))
							 1.0))


;; Exercise 1.46.  Several of the numerical methods described in this chapter are instances of an extremely general computational strategy known as iterative improvement. Iterative improvement says that, to compute something, we start with an initial guess for the answer, test if the guess is good enough, and otherwise improve the guess and continue the process using the improved guess as the new guess. Write a procedure iterative-improve that takes two procedures as arguments: a method for telling whether a guess is good enough and a method for improving a guess. Iterative-improve should return as its value a procedure that takes a guess as argument and keeps improving the guess until it is good enough. Rewrite the sqrt procedure of section 1.1.7 and the fixed-point procedure of section 1.3.3 in terms of iterative-improve.

(define (iterative-improve ge-method? improve)
	(lambda (guess) 
		(define (iter result) 
			(if (ge-method? result)
					result
				  (iter (improve result))))
		(iter guess)))

(define (sqrt x)
	(define (good-enough? guess)
		(< (abs (- (square guess) x)) 0.001))
	(define (improve guess)
   	(average guess (/ x guess)))
	((iterative-improve good-enough? improve) 1.0))

(define (fixed-point f)
	(define (improve x) (f x))
	(define (close-enough? guess)
    (< (abs (- guess (improve guess))) 0.0000001))
	((iterative-improve close-enough? improve) 1.0))
