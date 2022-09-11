(define (add-rat x y)
	(make-rat (+ (* (numer x) (denom y))
							 (* (numer y) (denom x)))
						(* (denom x) (denom y))))

(define (sub-rat x y)
	(make-rat (- (* (numer x) (denom y))
							 (* (numer y) (denom x)))
						(* (denom x) (denom y))))

(define (mult-rat x y)
	(make-rat (* (numer x) (numer y))
						(* (denom x) (denom y))))

(define (div-rat x y)
	(make-rat (* (numer x) (denom y))
						(* (denom x) *numer y)))

(define (equal-rat? x y)
	(= (* (numer x) (denom y))
		 (* (numer y) (denom x))))

(define (make-rat n d)
	(let ((g (gcd n d)))
		(cons (/ n g) (/ d g))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
	(newline)
	(display (numer x))
	(display "/") 
	(display (denom x)))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))

;; Exercise 2.1.  Define a better version of make-rat that handles both positive and negative arguments. Make-rat should normalize the sign so that if the rational number is positive, both the numerator and denominator are positive, and if the rational number is negative, only the numerator is negative.

(define (make-rat n d)
  (if (< d 0)
      (cons (* n -1) (* d -1))
      (cons n d)))

;; Exercise 2.2.  Consider the problem of representing line segments in a plane. Each segment is represented as a pair of points: a starting point and an ending point. Define a constructor make-segment and selectors start-segment and end-segment that define the representation of segments in terms of points. Furthermore, a point can be represented as a pair of numbers: the x coordinate and the y coordinate. Accordingly, specify a constructor make-point and selectors x-point and y-point that define this representation. Finally, using your selectors and constructors, define a procedure midpoint-segment that takes a line segment as argument and returns its midpoint (the point whose coordinates are the average of the coordinates of the endpoints). To try your procedures, you'll need a way to print points:

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment start end) (cons start end))
(define (start-segment segment)	(car segment))
(define (end-segment segment)	(cdr segment))
(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (midpoint-segment segment)
	(make-point (average (x-point (start-segment segment))
											 (x-point (end-segment segment)))
							(average (y-point (start-segment segment))
											 (y-point (end-segment segment)))))

(define start (make-point 1 1))
(define end (make-point 3 2))
(define my-segment (make-segment start end))

;; Exercise 2.3.  Implement a representation for rectangles in a plane. (Hint: You may want to make use of exercise 2.2.) In terms of your constructors and selectors, create procedures that compute the perimeter and the area of a given rectangle. Now implement a different representation for rectangles. Can you design your system with suitable abstraction barriers, so that the same perimeter and area procedures will work using either representation?

(define (area rectangle)
	(* (rectangle-height rectangle)
		 (rectangle-width rectangle)))

(define (perimeter rectangle)
	(+ (* 2 (rectangle-height rectangle))
		 (* 2 (rectangle-width rectangle))))

;; first representation: rectangle is a pair of height and width

(define (make-rectangle height width)
	(cons height width))
(define (rectangle-height rectangle) (car rectangle))
(define (rectangle-width rectangle) (cdr rectangle))

(define first-rectangle (make-rectangle 4 7))
;; (newline)
;; (display (area first-rectangle))
;; (newline)
;; (display (perimeter first-rectangle))

;; second representation: rectangle is a diagonal segment

(define (make-rectangle lower-left upper-right)
	(make-segment lower-left upper-right))
(define (rectangle-height rectangle) 
	(- (x-point (end-segment rectangle))
		 (x-point (start-segment rectangle))))
(define (rectangle-width rectangle)
	(- (y-point (end-segment rectangle))
		 (y-point (start-segment rectangle))))

(define second-rectangle (make-rectangle (make-point 1 1)
																				 (make-point 8 5)))
;; (newline)
;; (display (area second-rectangle))
;; (newline)
;; (display (perimeter second-rectangle))

;; (define (cons x y)
;; 	(define (dispatch m)
;; 		(cond ((= m 0) x)
;; 					((= m 1) y)
;; 					(else (error "Argument not 0 or 1 -- CONS" m))))
;; 	dispatch)

;; (cons 1 2)

(define my-pair (lambda (m) 
									(cond ((= m 0) 1)
												((= m 1) 2))))

(define (my-cons x y)
	(lambda (m) 
		(cond ((= m 0) x)
					((= m 1) y))))

(define (my-car z) (z 0))
(define (my-cdr z) (z 1))

;; Exercise 2.4.  Here is an alternative procedural representation of pairs. For this representation, verify that (car (cons x y)) yields x for any objects x and y.

(define (24-cons x y)
  (lambda (m) (m x y)))

(define (24-car z)
  (z (lambda (p q) p)))

;; What is the corresponding definition of cdr? (Hint: To verify that this works, make use of the substitution model of section 1.1.5.)

(define 24-pair (24-cons 1 2))
;; (lambda (m) (m 1 2))

;; (24-car 24-pair)
;; (24-pair (lambda (p q) p))

;; (lambda (m) (m 1 2) (lambda (p q) p))

;; ((lambda (p q) p) 1 2)

;; therefore

(define (24-cdr z)
	(z (lambda (p q) q)))


;; Exercise 2.5.  Show that we can represent pairs of nonnegative integers using only numbers and arithmetic operations if we represent the pair a and b as the integer that is the product 2^a * 3^b. Give the corresponding definitions of the procedures cons, car, and cdr.


(define (cons-num a b) (* (expt 2 a) (expt 3 b)))

(define (car-num x)
	(define (iter current result)
		(if (= (remainder current 2) 0)
				(iter (/ current 2) (inc result))
				result))
	(iter x 0))

(define (cdr-num x)
	(define (iter current result)
		(if (= (remainder current 3) 0)
				(iter (/ current 3) (inc result))
					result))
	(iter x 0))

(define pair-num-log (cons-num 1 2))

;; implementation using log (perhaps less iterations? depends on who log is implemented)

(define (car-num x)
	(define (iter current result)
		(if (= (remainder current 3) 0) 
				 (iter (/ current 3) result)
					(exact (/ (log current) (log 2)))))
	(iter x 0))

(define (cdr-num x)
	(define (iter current result)
		(if (= (remainder current 2) 0) 
				 (iter (/ current 2) result)
					(exact (/ (log current) (log 3)))))
	(iter x 0))

(define pair-num-log (cons-num 5 6))

;; Exercise 2.6.  In case representing pairs as procedures wasn't mind-boggling enough, consider that, in a language that can manipulate procedures, we can get by without numbers (at least insofar as nonnegative integers are concerned) by implementing 0 and the operation of adding 1 as

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; This representation is known as Church numerals, after its inventor, Alonzo Church, the logician who invented the lambda calculus.

;; Define one and two directly (not in terms of zero and add-1). (Hint: Use substitution to evaluate (add-1 zero)). Give a direct definition of the addition procedure + (not in terms of repeated application of add-1).

(add-1 zero)

(add-1 (lambda (f) (lambda (x) x)))

(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))

;; the middle part evaluates to x
;; ((lambda (f) (lambda (x) x)) f) x) ;; apply f to this gives
;; ((lambda (x) x) x)
;; x

;; testing:
;; 1 ]=> (((lambda (f) (lambda (x) x)) f) 3)
;; ;Value: 3

;; (lambda (f) (lambda (x) (f x))) ;; this is one

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

;; ;; transform one and two somehow into
;; (lambda (f) (lambda (x) (f (f (f x)))))

;; (lambda (f) (lambda (x) ((lambda (x) (f (f x))) (f x))))
;;                           ---------------------- -----  
;;              						 this is (two f)         this is ((one f) x)

;; three can be written as
;; (lambda (f)
;; 	(lambda (x) ((two f) ((one x) f))))

(define (church+ a b)
	(lambda (f) (lambda (x) ((a f) ((b f) x)))))

;; ;; testing

;; (church+ two one)

;; (church+ (lambda (f) (lambda (x) (f (f x))))
;; 				 	(lambda (f) (lambda (x) (f x))))


;; (lambda (f) 
;; 	(lambda (x) 
;; 		(((lambda (f) (lambda (x) (f (f x)))) f) 
;; 						 (((lambda (f) (lambda (x) (f x))) f) x))))

;; ; substituting f

;; (lambda (f) 
;; 	(lambda (x)
;; 		((lambda (x) (f (f x)))
;; 		 ((lambda (x) (f x)) x))))

;; ; substituting x

;; (lambda (f) 
;; 	(lambda (x) 
;; 		((lambda (x) (f (f x))) (f x))))

;; ; applying (f x) to the first procedure

;; (lambda (f) 
;; 	(lambda (x) 
;; 		(f (f (f x)))))


;;  2.1.4  Extended Exercise: Interval Arithmetic

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; 	Exercise 2.7.  Alyssa's program is incomplete because she has not specified the implementation of the interval abstraction. Here is a definition of the interval constructor:

(define (make-interval a b) (cons a b))

;; Define selectors upper-bound and lower-bound to complete the implementation.

(define (upper-bound int) (cdr int))
(define (lower-bound int) (car int))

;; Exercise 2.8.  Using reasoning analogous to Alyssa's, describe how the difference of two intervals may be computed. Define a corresponding subtraction procedure, called sub-interval.

; I think there is no negative resistence, therefore we need to select the higher number to subtract

(define (sub-interval x y)
	(let ((max-lower (max (lower-bound x) (lower-bound y)))
				(min-lower (min (lower-bound x) (lower-bound y)))
				(max-upper (max (upper-bound x) (upper-bound y)))
				(min-upper (min (upper-bound x) (upper-bound y))))
		(make-interval (- max-lower min-lower)
									 (- max-upper min-upper))))

(define res1 (make-interval 6.12 7.48))
(define res2 (make-interval 4.46 4.93))

;; Exercise 2.9.  The width of an interval is half of the difference between its upper and lower bounds. The width is a measure of the uncertainty of the number specified by the interval. For some arithmetic operations the width of the result of combining two intervals is a function only of the widths of the argument intervals, whereas for others the width of the combination is not a function of the widths of the argument intervals. Show that the width of the sum (or difference) of two intervals is a function only of the widths of the intervals being added (or subtracted). Give examples to show that this is not true for multiplication or division.

(define (width interval)
	(/ (- (upper-bound interval) 
				(lower-bound interval))
		 2))

;; (newline)
;; (display (width res1))
;; (newline)
;; (display (width res2))
;; (newline)
;; (display (width (add-interval res1 res2)))
;; (newline)
;; (display (= (+ (width res1) (width res2)) (width (add-interval res1 res2))))
;; (newline)
;; (display (width (sub-interval res1 res2)))
;; (newline)
;; (display (= (- (width res1) (width res2)) (width (sub-interval res1 res2))))
;; (newline)
;; (display (width (mul-interval res1 res2)))
;; (newline)
;; (display (= (* (width res1) (width res2)) (width (mul-interval res1 res2))))
;; (newline)
;; (display (width (div-interval res1 res2)))
;; (newline)
;; (display (= (/ (width res1) (width res2)) (width (div-interval res1 res2))))

;; Exercise 2.10.  Ben Bitdiddle, an expert systems programmer, looks over Alyssa's shoulder and comments that it is not clear what it means to divide by an interval that spans zero. Modify Alyssa's code to check for this condition and to signal an error if it occurs.

(define (div-interval x y)
	(define (spans-zero? int)
		(if (<= (* (lower-bound int) (upper-bound int)) 0) 
				#t	
				#f))
	(if (spans-zero? y)
			(error "Trying to divide by an interval that spans 0 ---" y)
  		(mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

(define res3 (make-interval -1 2))

;; Exercise 2.11.  In passing, Ben also cryptically comments: ``By testing the signs of the endpoints of the intervals, it is possible to break mul-interval into nine cases, only one of which requires more than two multiplications.'' Rewrite this procedure using Ben's suggestion.

(define (mul-interval x y)
  (let ((lbx (lower-bound x)) (ubx (upper-bound x))
				(lby (lower-bound y)) (uby (upper-bound y)))
		(cond ((and (> lbx 0) (> ubx 0))
					 (cond ((and (> lby 0) (> uby 0)) (make-interval (* lbx lby)
																													 (* ubx uby)))
								 ((and (< lby 0) (> uby 0)) (make-interval (* ubx lby)
																													 (* ubx uby)))
								 ((and (< lby 0) (< uby 0)) (make-interval (* ubx lby)
																													 (* lbx uby)))))
					((and (< lbx 0) (> ubx 0)) 
					 (cond ((and (> lby 0) (> uby 0)) (make-interval (* lbx uby)
																													 (* ubx uby)))
								 ((and (< lby 0) (> uby 0)) (make-interval (min (* lbx uby)
																																(* ubx lby))
																													 (max (* ubx uby)
																																(* lbx lby))))
								 ((and (< lby 0) (< uby 0)) (make-interval (* ubx lby)
																													 (* lbx lby)))))
					((and (< lbx 0) (< lby 0))
					 (cond ((and (> lby 0) (> uby 0)) (make-interval (* lbx uby)
																													 (* ubx lby)))
								 ((and (< lby 0) (> uby 0)) (make-interval (* lbx uby)
																													 (* lbx lby)))
								 ((and (< lby 0) (< uby 0)) (make-interval (* ubx uby)
																													 (* lbx lby))))))))

;; After debugging her program, Alyssa shows it to a potential user, who complains that her program solves the wrong problem. He wants a program that can deal with numbers represented as a center value and an additive tolerance; for example, he wants to work with intervals such as 3.5 ± 0.15 rather than [3.35, 3.65]. Alyssa returns to her desk and fixes this problem by supplying an alternate constructor and alternate selectors:

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; Unfortunately, most of Alyssa's users are engineers. Real engineering situations usually involve measurements with only a small uncertainty, measured as the ratio of the width of the interval to the midpoint of the interval. Engineers usually specify percentage tolerances on the parameters of devices, as in the resistor specifications given earlier.

;; Exercise 2.12.  Define a constructor make-center-percent that takes a center and a percentage tolerance and produces the desired interval. You must also define a selector percent that produces the percentage tolerance for a given interval. The center selector is the same as the one shown above.

(define (make-center-percent c %)
	(let ((width (* c %)))
		(make-interval (- c width) (+ c width))))

(define (percent i)
	(/ (- (center i) (lower-bound i)) (center i)))

;; Exercise 2.13.  Show that under the assumption of small percentage tolerances there is a simple formula for the approximate percentage tolerance of the product of two intervals in terms of the tolerances of the factors. You may simplify the problem by assuming that all numbers are positive.

;; (lower-bound (mul-interval x y))
;; (* (lower-bound x) (lower-bound y))

;; (Cx - CxTx) (Cy - CyTy)
;; CxCy - CxCyTy - CxCyTx + CxCyTxTy
;; CxCy - CxCy (Tx + Ty - TxTy)
;;             ----------------
;; 						 new tolerance

;; test

(define tol1 (make-center-percent 6.8 0.1))
(define tol2 (make-center-percent 4.7 0.05))
(define tol3 (mul-interval tol1 tol2))

;; (newline)
;; (display (+ (percent tol1)
;; 						(percent tol2)
;; 						(- (* (percent tol1) (percent tol2))))) ; .14500000000000002
;; (newline)
;; (display (percent tol3)) ; .14925373134328349
;; 
;; close enough?

;; After considerable work, Alyssa P. Hacker delivers her finished system. Several years later, after she has forgotten all about it, she gets a frenzied call from an irate user, Lem E. Tweakit. It seems that Lem has noticed that the formula for parallel resistors can be written in two algebraically equivalent ways:

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/ch2-Z-G-9.gif

;; and

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/ch2-Z-G-10.gif

;; He has written the following two programs, each of which computes the parallel-resistors formula differently:

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;; Lem complains that Alyssa's program gives different answers for the two ways of computing. This is a serious complaint.

;; Exercise 2.14.  Demonstrate that Lem is right. Investigate the behavior of the system on a variety of arithmetic expressions. Make some intervals A and B, and use them in computing the expressions A/A and A/B. You will get the most insight by using intervals whose width is a small percentage of the center value. Examine the results of the computation in center-percent form (see exercise 2.12).

(define tol4 (make-center-percent 5.2 0.05))
(define tol5 (make-center-percent 6.7 0.03))

;; 1 ]=> (par1 tol4 tol5)
;; ;Value: (2.597286627295527 . 3.2939470233412007)

;; 1 ]=> (par2 tol4 tol5)
;; ;Value: (2.8066316985750506 . 3.04825337755845)

;; 1 ]=> (par1 tol2 tol3)
;; ;Value: (2.9154885444743934 . 5.730261679479598)

;; 1 ]=> (par2 tol2 tol3)
;; ;Value: (3.837893258426966 . 4.353042452830189)

;; 2 error> (par1 tol2 tol2)
;; ;Value: (2.0198809523809524 . 2.727236842105264)

;; 2 error> (par1 tol5 tol5)
;; ;Value: (3.060208737864078 . 3.6639329896907213)

;; 2 error> (par2 tol5 tol5)
;; ;Value: (3.2495000000000003 . 3.4505)

;; Exercise 2.15.  Eva Lu Ator, another user, has also noticed the different intervals computed by different but algebraically equivalent expressions. She says that a formula to compute with intervals using Alyssa's system will produce tighter error bounds if it can be written in such a form that no variable that represents an uncertain number is repeated. Thus, she says, par2 is a ``better'' program for parallel resistances than par1. Is she right? Why?

(define (par1-test1 i1 i2)
(/ (* (lower-bound i1) (lower-bound i2))
	 (+ (upper-bound i1) (upper-bound i2))))

(define (par1-test2 i1 i2)
	(/ (* (upper-bound i1) (upper-bound i2))
	 (+ (lower-bound i1) (upper-bound i2))))

;; 1 ]=> (par1-test1 tol4 tol5)

;; ;Value: 2.5972866272955266

;; 1 ]=> (par1-test2 tol4 tol5)

;; ;Value: 3.1821180643526725

(define (par2-test1 i1 i2)
	(/ (* (lower-bound i1) (lower-bound i2))
	 (+ (lower-bound i1) (lower-bound i2))))

(define (par2-test2 i1 i2)
(/ (* (upper-bound i1) (upper-bound i2))
	 (+ (upper-bound i1) (upper-bound i2))))

;; 1 ]=> (par2-test1 tol4 tol5)
;; ;Value: 2.806631698575051

;; 1 ]=> (par2-test2 tol4 tol5)
;; ;Value: 3.0482533775584497


;;; seems like each of them give different results

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

;; (div-interval ((* l1 l2), (* u1 u2))
;; 							((+ l1 l2), (+ u1 u2)))

;; (mul-interval ((* l1 l2), (* u1 u2))
;; 							((/ 1.0 (+ u1 u2)), (/ 1.0 (+ l1 l2))))

;; ((/ (* l1 l2) (+ u1 u2)), 
;;  (/ (* u1 u2) (+ l1 l2)))

;; (define (par2 r1 r2)
;;   (let ((one (make-interval 1 1))) 
;;     (div-interval one
;;                   (add-interval (div-interval one r1)
;;                                 (div-interval one r2)))))

;; (div-interval (1,1)
;; 							 (add-interval (div-interval (1,1) (l1,u1))
;;                              (div-interval (1,1) (l2,u2))))

;; (div-interval (1,1)
;; 							(add-interval (1/u1,1/l1) (1/u2,1/l2)))

;; (div-interval (1,1)
;; 							((+ 1/u1,1/u2) (+ 1/l1,1/l2)))

;; (div-interval (1,1)
;; 							((/ (+ u1 u2) (* u1 u2)),(/ (+ l1 l2) (* l1 l2))))

;; (mul-interval (1,1)
;; 							((/ (* l1 l2) (+ l1 l2)), (/ (* u1 u2) (+ u1 u2))))

;; ((/ (* l1 l2) (+ l1 l2)), 
;;  (/ (* u1 u2) (+ u1 u2)))

;;;;; from http://community.schemewiki.org/?sicp-ex-2.14-2.15-2.16

;; All 3 problems point to the difficulty of "identity" when dealing with intervals. Suppose we have two numbers A and B which are contained in intervals:

;;   A = [2, 8]
;;   B = [2, 8]

;; A could be any number, such as 3.782, and B could be 5.42, but we just don't know.

;; Now, A divided by itself must be 1.0 (assuming A isn't 0), but of A/B (the same applies to subtraction) we can only say that it's somewhere in the interval

;;   [0.25, 4]
;; Unfortunately, our interval package doesn't say anything about identity, so if we calculated A/A, we would also get

;;   [0.25, 4]
;; So, any time we do algebraic manipulation of an equation involving intervals, we need to be careful any time we introduce the same interval (e.g. through fraction reduction), since our interval package re-introduces the uncertainty, even if it shouldn't.

;; So:

;; 2.14. Lem just demonstrates the above.

;; 2.15. Eva is right, since the error isn't reintroduced into the result in par2 as it is in par1.

;; 2.16. A fiendish question. They say it's "very difficult" as if it's doable. I'm not falling for that. Essentially, I believe we'd have to introduce some concept of "identity", and then have the program be clever enough to reduce equations. Also, when supplying arguments to any equation, we'd need to indicate identity somehow, since [2, 8] isn't necessarily the same as [2, 8] ... unless it is. Capiche?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; one interesting thing could be to define add, sub, mul and divide as operations with centers and tolerances. We could store intervals as cons of centers and tolerances and the selectors would be center and tolerance (in %)

(define make-interval cons)
(define center car)
(define tolerance cdr)

(define (add-int r1 r2)
	(make-interval (+ (center r1) (center r2))
								 (+ (* (center r1) (tolerance r1)) (center r2) (tolerance r2))))

(define (sub-int r1 r2)
	(let* ((c1 (max (center r1) (center r2)))
				 (c2 (min (center r1) (center r2)))
				 (t1 (if (= c1 r1) (tolerance r1) (tolerance r2)))
				 (t1 (if (= c2 r2) (tolerance r2) (tolerance r1))))
		    (make-interval (- c1 c2)
											 (- (* c1 t1) (* c2 t2)))))

(define (mul-int r1 r2) ;; supposing all numbers are positive
	(make-interval (* (center r1) (center r2))
								 (+ (tolerance r1) 
										(tolerance r2) 
										(* (tolerance r1) (tolerance r2)))))

(define (div-int r1 r2)
	(make-interval (/ (center r1) (center r2))
								 (if (= (tolerance r1) 0) ;; what it means do divide by 0?
										 (tolerance r2)
										 (/ (* (center r1) (tolerance r1))
												(* (center r2) (tolerance r2))))))
								 

(define (par1 r1 r2)
  (div-int (mul-int r1 r2)
           (add-int r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1.0 0.0)))  ;; how to define one in this case?
    (div-int one
                  (add-int (div-int one r1)
                           (div-int one r2)))))

(define resA (make-interval 4.0 0.05))
(define resB (make-interval 3.0 0.1))

;; 1 ]=> (par1 resA resB)

;; ;Value: (1.7142857142857142 . .08051948051948053)

;; 1 ]=> (par2 resA resB)

;; ;Value: (1.7142857142857144 . .4458333333333333)

;; same center, but different tolerances.