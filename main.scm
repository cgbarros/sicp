(load "sec1.3.com")
(load "sec2.1.com")
(load "sec2.2.com")
(load "sec2.3.com")
(load "sec2.4.com")

;; Generic Arithmetic Operations

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (+ x y)))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures from rectangular 
  ;; and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 
          'rectangular) 
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) 
     r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag 
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag 
     (- (real-part z1) (real-part z2))
     (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang 
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang 
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) 
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) 
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) 
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) 
         (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
	(put 'real-part '(complex) real-part)
	(put 'imag-part '(complex) imag-part)
	(put 'magnitude '(complex) magnitude)
	(put 'angle '(complex) angle)
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

;; Exercise 2.77: Louis Reasoner tries to evaluate the expression (magnitude z) where z is the object shown in Figure 2.24. To his surprise, instead of the answer 5 he gets an error message from apply-generic, saying there is no method for the operation magnitude on the types (complex). He shows this interaction to Alyssa P. Hacker, who says “The problem is that the complex-number selectors were never defined for complex numbers, just for polar and rectangular numbers. All you have to do to make this work is add the following to the complex package:”

;; (put 'real-part '(complex) real-part)
;; (put 'imag-part '(complex) imag-part)
;; (put 'magnitude '(complex) magnitude)
;; (put 'angle '(complex) angle)

;; Describe in detail why this works. As an example, trace through all the procedures called in evaluating the expression (magnitude z) where z is the object shown in Figure 2.24. In particular, how many times is apply-generic invoked? What procedure is dispatched to in each case?

;;;; the definition of magnitude is

(define (magnitude z) 
  (apply-generic 'magnitude z))

;; for some reason I had to define it again

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types: 
             APPLY-GENERIC"
            (list op type-tags))))))

;; (define z (make-complex-from-real-imag 3 4))
;; (define my-magnitude (get 'magnitude '(complex)))
;; (display (my-magnitude z))

;; (magnitude z)
;; (apply-generic 'magnitude '(complex rectangular 3 . 4))
;; ((get 'magnitude '(complex)) '(rectangular 3 . 4))
;; (magnitude '(rectangular 3 . 4))
;; (apply-generic 'magnitude '(rectangular 3 . 4))
;; ((get 'magnitude '(rectangular)) '(3 . 4))
; Value: 5.000023178253949


;; This works because magnitude is available outside as apply-generic
;; since when we create the complex number we attach the number within it
;; all that (get 'magnitude '(complex)) does is to call magnitude again with
;; the correct type
;; in the exmple, apply-generic is calles twice: first, it dispatches
;; (get 'magnitude '(complex)), which calls apply-generic again, but now
;; dispatches (get 'magnitude '(rectangular))

;; Exercise 2.78: The internal procedures in the scheme-number package are essentially nothing more than calls to the primitive procedures +, -, etc. It was not possible to use the primitives of the language directly because our type-tag system requires that each data object have a type attached to it. In fact, however, all Lisp implementations do have a type system, which they use internally. Primitive predicates such as symbol? and number? determine whether data objects have particular types. Modify the definitions of type-tag, contents, and attach-tag from 2.4.2 so that our generic system takes advantage of Scheme’s internal type system. That is to say, the system should work as before except that ordinary numbers should be represented simply as Scheme numbers rather than as pairs whose car is the symbol scheme-number.

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
				((pair? datum) (car datum))
      	(else (error "Bad tagged datum: 
        	      TYPE-TAG" datum))))

(define (attach-tag type-tag contents)
	(if (number? contents)
			contents
			(cons type-tag contents)))

(define (contents datum)
  (cond ((number? datum) datum)
				((pair? datum) (cdr datum))
      	(else (error "Bad tagged datum: 
          	    CONTENTS" datum))))

;; (display (add 1 2)) (newline)
;; (display (sub 5 2)) (newline)
;; (display (mul 7 8)) (newline)
;; (display (div 8 2)) (newline)

;; Exercise 2.79: Define a generic equality predicate equ? that tests the equality of two numbers, and install it in the generic arithmetic package. This operation should work for ordinary numbers, rational numbers, and complex numbers.

; this works, but it can't compare a rectangular complex number with a magnitude complex number
(define (equ? x y)
	(and (eq? (type-tag x) (type-tag y))
			 (equal? (contents x) (contents y))))

;;;;;; other solution

;; 	;; number package:
;; (put 'equ? '(scheme-number scheme-number) =)

;; ;; rational package:
;; (define (equ? x y) 
;;  (and (= (numer x) (numer y)) (= (denom x) (denom y)))) 
 
;; (put 'equ? '(rational rational) equ?)
;; ;; complex package:
;; (define (equ? z1 z2) 
;;  (and (= (apply-generic 'real-part z1) (apply-generic 'real-part z2))
;;       (= (apply-generic 'imag-part z1) (apply-generic 'imag-part z2)))) 

;; (put 'equ? '(complex complex) equ?))

;; ; generic definition
;; (define (equ? x y)
;;   (apply-generic 'equ? x y)))


(define half (make-rational 1 2))
(define third (make-rational 1 3))
(define complexA (make-complex-from-real-imag 2 3))
(define complexB (make-complex-from-mag-ang 1 2))

;; Exercise 2.80: Define a generic predicate =zero? that tests if its argument is zero, and install it in the generic arithmetic package. This operation should work for ordinary numbers, rational numbers, and complex numbers.

(define (=zero? n)
	(cond ((eq? (type-tag n) 'scheme-number) (= n 0))
				; this is a violation of the abstraction bariers:
			  ((eq? (type-tag n) 'rational) (= (car (contents n)) 0))
				((eq? (type-tag n) 'complex) (and (= (cadr (contents n)) 0) 
					 																(= (cddr (contents n)) 0)))
				(else (error "Type uknown: =ZERO?" n))))

;;;;;; other solution

;; ;; number package:
;; (put '=zero? '(scheme-number scheme-number) (lambda (x) (= x 0)))

;; ;; rational package:
;; (define (=zero? x) 
;;   (= (numer x) 0))
 
;; (put '=zero? '(rational) =zero?)

;; ;; complex package:
;; (define (=zero? z) 
;;  (and (= (apply-generic 'real-part z) 0)
;;       (= (apply-generic 'imag-part z) 0)))

;; (put '=zero? '(complex) =zero?)

;; ; generic definition
;; (define (=zero? x)
;;   (apply-generic '=zero? x))

(define zero-rational (make-rational 0 10))
(define zero-complex-rect (make-complex-from-real-imag 0 0))
(define zero-complex-ang  (make-complex-from-mag-ang 0 0))

;; Coercion

;;;;;; some definitions to make the code work
(define *coercion-table* (make-equal-hash-table))

(define (put-coercion typeA typeB proc)
 	(hash-table-set! *coercion-table* (list typeA typeB) proc))

(define (get-coercion typeA typeB)
 	(hash-table-ref/default *coercion-table* (list typeA typeB) #f))
;;;;;;

(define (scheme-number->complex n)
  (make-complex-from-real-imag 
   (contents n) 0))

(put-coercion 'scheme-number 'complex 
              scheme-number->complex)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 
                       (get-coercion type1
                                     type2))
                      (t2->t1 
                       (get-coercion type2 
                                     type1)))
                  (cond (t1->t2
                         (apply-generic 
                          op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic 
                          op a1 (t2->t1 a2)))
                        (else
                         (error 
                          "No method for these types"
                          (list 
                           op 
                           type-tags))))))
              (error 
               "No method for these types"
               (list op type-tags)))))))

;; Hierarchies of types and Inadequacies of hierarchies

;; Exercise 2.81: Louis Reasoner has noticed that apply-generic may try to coerce the arguments to each other’s type even if they already have the same type. Therefore, he reasons, we need to put procedures in the coercion table to coerce arguments of each type to their own type. For example, in addition to the scheme-number->complex coercion shown above, he would do:

;; (define (scheme-number->scheme-number n) n)
;; (define (complex->complex z) z)

;; (put-coercion 'scheme-number 'scheme-number
;;               scheme-number->scheme-number)

;; (put-coercion 'complex 'complex 
;;               complex->complex)

;; 1. With Louis’s coercion procedures installed, what happens if apply-generic is called with two arguments of type scheme-number or two arguments of type complex for an operation that is not found in the table for those types? For example, assume that we’ve defined a generic exponentiation operation:

(define (exp x y) 
  (apply-generic 'exp x y))

;; and have put a procedure for exponentiation in the Scheme-number package but not in any other package:

;; following added to Scheme-number package
(define (install-exp-scheme-number)
	(define (tag x)
    (attach-tag 'scheme-number x))
	(put 'exp 
     '(scheme-number scheme-number)
     (lambda (x y) 
       (tag (expt x y)))))
       ; using primitive expt

(install-exp-scheme-number)

			 
;; What happens if we call exp with two complex numbers as arguments?

;;;; since apply-generic tests for proc, if it's not found it will try to coerce complex to complex forever and there will be a stack overflow
;;;; testing with 1 ]=> (exp complexA complexA) froze the system

;; 2. Is Louis correct that something had to be done about coercion with arguments of the same type, or does apply-generic work correctly as is?

;;;; Nothing had to be done.
;;;; -- apply generic will not find proc
;;;; -- it will try to coerce
;;;; -- it will not find how to coerce
;;;; -- it will fail
;;;; He is correct that apply-generic will try to coerce two arguments of the same type, but it will work regardeless


;; 3. Modify apply-generic so that it doesn’t try coercion if the two arguments have the same type.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (and (= (length args) 2) 
									 (not (equal? (car type-tags) (cadr type-tags)))) ;; new line
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 
                       (get-coercion type1
                                     type2))
                      (t2->t1 
                       (get-coercion type2 
                                     type1)))
                  (cond (t1->t2
                         (apply-generic 
                          op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic 
                          op a1 (t2->t1 a2)))
                        (else
                         (error 
                          "Tried coercion. No method for these types"
                          (list 
                           op 
                           type-tags))))))
              (error 
               "Didn't tried coercion. No method for these types"
               (list op type-tags)))))))

;; Exercise 2.82: Show how to generalize apply-generic to handle coercion in the general case of multiple arguments. One strategy is to attempt to coerce all the arguments to the type of the first argument, then to the type of the second argument, and so on. Give an example of a situation where this strategy (and likewise the two-argument version given above) is not sufficiently general. (Hint: Consider the case where there are some suitable mixed-type operations present in the table that will not be tried.)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags))
				  (all-equal? (filter not (map (lambda (x) (equal? x (car type-tags))) type-tags))))
			(display all-equal?) (newline)
			(define (try-coercion t1 t2 last-coercion)
						(cond ((null? t2) (error "Tried coercion. No method for these types"))
									((null? t1) 
									 (display "the selected coercion is") last-coercion)
									;; (aplly-generic op (map (get-coercion (car t1) (car t2)) args)) ;; this would be the correct call here
									(else (let ((current-coercion (get-coercion (car t1) (car t2))))
													(cond ((equal? (car t1) (car t2))
																 (display "(car t1) and (car t2) are equal") (newline)
																 (try-coercion (cdr t1) t2 last-coercion))
																((not current-coercion) 
																 (display "no method for current-coercion") (newline)
																 (try-coercion type-tags (cdr t2) current-coercion))
																(else 
																 (display "this one works, trying next one") (newline)
																			(try-coercion (cdr t1) t2 current-coercion)))))))
      (if proc
          (apply proc (map contents args))
					(if (null? all-equal?)
							(error "Didn't try coercion. All are equal and there's no method.")
							(try-coercion type-tags 
														type-tags 
														(get-coercion (car type-tags) (car type-tags))))))))

;; complex rational integer real
;; in this case, we won't try to coerce anything into rational
;; supposing we would need to first convert integer to real, then real to rational and finally rational do complex. In this case the procedure above wouldn't work except if we had a system o tower coercion

;; another case from https://www.inchmeal.io/sicp/ch-2/ex-2.82.html
;; Consider the case that we have an operation defined that only work for rational numbers, now if we pass two numbers - then ideally we would expect that both numbers are coerced into rational numbers and then the operation is applied but our implementation does not support this.

;; Exercise 2.83: Suppose you are designing a generic arithmetic system for dealing with the tower of types shown in Figure 2.25: integer, rational, real, complex. For each type (except complex), design a procedure that raises objects of that type one level in the tower. Show how to install a generic raise operation that will work for each type (except complex).

(define numer cadr)
(define denom cddr)

(define (install-raise-package)
(define (scheme-number->rational n)
	(make-rational (contents n) 1))

(define (rational->real n)
 	(list 'real (/ (numer n) (denom n))))

(define (real->complex n)
	(make-complex-from-real-imag (cadr n) 0))

(put 'raise 'scheme-number scheme-number->rational)
(put 'raise 'rational rational->real)
(put 'raise 'real real->complex))

(install-raise-package)

(define (raise number)
	(let ((proc (get 'raise (type-tag number))))
		(if proc
				(proc number)
				#f)))

;; Exercise 2.84: Using the raise operation of Exercise 2.83, modify the apply-generic procedure so that it coerces its arguments to have the same type by the method of successive raising, as discussed in this section. You will need to devise a way to test which of two types is higher in the tower. Do this in a manner that is “compatible” with the rest of the system and will not lead to problems in adding new levels to the tower.

;; to make it simpler: go back to two types only

(define (higher n1 n2)
	(let ((type1 (type-tag n1))
				(type2 (type-tag n2))
				(raised1 (raise n1))
				(raised2 (raise n2)))
	(cond ((eq? type1 type2) 0)
				((not raised1) 1)
				((not raised2) 2)
				(else (higher raised1 raised2)))))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (and (= (length args) 2) 
									 (not (equal? (car type-tags) (cadr type-tags))))
              (let ((a1 (car args))
                    (a2 (cadr args)))
                (let ((higher (higher a1 a2)))
									(cond ((= higher 1) (apply-generic op a1 (raise a2)))
												((= higher 2) (apply-generic op (raise a1) a2))
												(else (error "Tried coercion. No method for these types")))))
							(error 
		              "Didn't tried coercion. No method for these types"
		              (list op type-tags)))))))

;; Exercise 2.85: This section mentioned a method for “simplifying” a data object by lowering it in the tower of types as far as possible. Design a procedure drop that accomplishes this for the tower described in Exercise 2.83. The key is to decide, in some general way, whether an object can be lowered. For example, the complex number 1.5+0i can be lowered as far as real, the complex number 1+0i can be lowered as far as integer, and the complex number 2+3i cannot be lowered at all. Here is a plan for determining whether an object can be lowered: Begin by defining a generic operation project that “pushes” an object down in the tower. For example, projecting a complex number would involve throwing away the imaginary part. Then a number can be dropped if, when we project it and raise the result back to the type we started with, we end up with something equal to what we started with. Show how to implement this idea in detail, by writing a drop procedure that drops an object as far as possible. You will need to design the various projection operations and install project as a generic operation in the system. You will also need to make use of a generic equality predicate, such as described in Exercise 2.79. Finally, use drop to rewrite apply-generic from Exercise 2.84 so that it “simplifies” its answers.

;; Begin by defining a generic operation project that “pushes” an object down in the tower.

(define (install-drop-package)
	(define (complex->real n)
			(list 'real (real-part n)))
	
	(define (real->rational n)
		(make-rational (numerator (cadr n)) (denominator (cadr n))))
	
	(define (rational->scheme-number n)
		(numer n))			
				

(put 'project 'complex complex->real)
(put 'project 'real real->rational)
(put 'project 'rational rational->scheme-number)
;; (put 'project 'polynomial polynomial->number)
	'done)
(install-drop-package)

(define (project number)
	(if (or (pair? number) (number? number))
		(let ((proc (get 'project (type-tag number))))
			(if proc
					(proc number)
					#f))
	#f))

 (define mycomplex (make-complex-from-real-imag 2.3 0))
 (define myrational (project (project mycomplex)))

;; a number can be dropped if, when we project it and raise the result back to the type we started with, we end up with something equal to what we started with.

;; (display (equ? (raise (project complexA)) complexA)) (newline)

;; (define falseComplex (make-complex-from-real-imag 3 0))
;; (display (equ? (raise (project falseComplex)) falseComplex)) (newline)

;; (define myRational (make-rational 2 3))
;; (display (equ? (raise (project myRational)) myRational)) (newline)

;; (define falseRational (make-rational 2 1))
;; (display (equ? (raise (project falseRational)) falseRational)) (newline)

;; Design a procedure drop that lowers the data object in the tower of types as far as possible

(define (drop data-object)
	(let ((projected-data (project data-object)))
		(if (and projected-data 
						 (equ? data-object (raise projected-data)))
				(drop projected-data)
				data-object)))

;; Finally, use drop to rewrite apply-generic from Exercise 2.84 so that it “simplifies” its answers.

(define (apply-generic op . args)
	;(display "running apply-generic with ") (display args) (newline)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop	(apply proc (map contents args)))
          (if (= (length args) 2)
               (let ((a1 (car args))   ; simplifying
                     (a2 (cadr args))) ; the arguments
                (let ((higher (higher a1 a2)))
									;(display "higher is ") (display higher) (newline)
									(cond ((= higher 1) (apply-generic op a1 (raise a2)))
												((= higher 2) (apply-generic op (raise a1) a2))
												(else (error "Tried coercion. No method for these types")))))
							(error 
		              "Didn't tried coercion. No method for these types"
		              (list op type-tags)))))))

;; Exercise 2.86: Suppose we want to handle complex numbers whose real parts, imaginary parts, magnitudes, and angles can be either ordinary numbers, rational numbers, or other numbers we might wish to add to the system. Describe and implement the changes to the system needed to accommodate this. You will have to define operations such as sine and cosine that are generic over ordinary numbers and rational numbers.

;; a complex number might look like this: '(complex rectangular (rational 2 3) . 4)

;; in fact, we can already do something like
;; 1 ]=> (make-complex-from-real-imag '(rational 2 3) 4)
;; ;Value: (complex rectangular (rational 2 3) . 4)

;; 1 ]=> (make-complex-from-mag-ang '(rational 2 3) '(real 4.3))
;; ;Value: (complex polar (rational 2 3) real 4.3)

(define (install-generic-complex-package)
  ;; imported procedures from rectangular 
  ;; and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 
          'rectangular) 
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) 
     r a))
	
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag 
     (add (real-part z1) (real-part z2))
     (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag 
     (sub (real-part z1) (real-part z2))
     (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang 
     (mul (magnitude z1) (magnitude z2))
     (mul (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang 
     (div (magnitude z1) (magnitude z2))
     (sub (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) 
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) 
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) 
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) 
         (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
	(put 'real-part '(complex) real-part)
	(put 'imag-part '(complex) imag-part)
	(put 'magnitude '(complex) magnitude)
	(put 'angle '(complex) angle)
  'done)

	(define scheme-sin sin)
	(define scheme-cos cos)
	(define scheme-atan atan)
	(define scheme-square square)

(define (install-generic-operations-package)
	;; in reality each package needs to add it's own sin, cos,etc.
	
	(define (rational-sin x) (attach-tag 'real (scheme-sin (/ (car x) (cdr x)))))
	(define (rational-cos x) (attach-tag 'real (scheme-cos (/ (car x) (cdr x)))))
	(define (rational-atan x) (attach-tag 'real (scheme-atan (/ (car x) (cdr x)))))
	(define (rational-square x) (attach-tag 'real (scheme-square (/ (car x) (cdr x)))))

  (put 'sin '(rational) rational-sin)
  (put 'cos '(rational) rational-cos)
  (put 'atan '(rational) rational-atan)
	(put 'square '(rational) rational-square)
	
  (put 'sin '(scheme-number) (lambda (x) (attach-tag 'real (scheme-sin x))))
  (put 'cos '(scheme-number) (lambda (x) (attach-tag 'real (scheme-cos x))))
  (put 'atan '(scheme-number) (lambda (x) (attach-tag 'real (scheme-atan x))))
	(put 'square '(scheme-number) (lambda (x) (attach-tag 'scheme-number (scheme-square x))))

	(put 'sin '(real) (lambda (x) (attach-tag 'real (scheme-sin (cadr x)))))
  (put 'cos '(real) (lambda (x) (attach-tag 'real (scheme-cos (cadr x)))))
	(put 'atan '(real) (lambda (x) (attach-tag 'real (scheme-atan (cadr x)))))
	(put 'square '(real) (lambda (x) (attach-tag 'real (scheme-square (cadr x))))))

(install-generic-operations-package)

(define (sin x) (apply-generic 'sin x))
(define (cos x) (apply-generic 'cos x))
(define (atan x) (apply-generic 'atan x))
(define (square x) (apply-generic 'square x))
			
(define rational-complex (make-complex-from-real-imag 
													(make-rational 2 3)
													4))

(define my-real '(real 2.3))

;; Arithmetic on polynomials


(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
	(define (make-term order coeff) 
  	(list order coeff))
	
  (define (add-poly p1 p2)
  (if (same-variable? (variable p1) 
                      (variable p2))
      (make-poly 
       (variable p1)
       (add-terms (term-list p1)
                  (term-list p2)))
      (error "Polys not in same var: 
              ADD-POLY"
             (list p1 p2))))
	
 (define (mul-poly p1 p2)
  (if (same-variable? (variable p1) 
                      (variable p2))
      (make-poly 
       (variable p1)
       (mul-terms (term-list p1)
                  (term-list p2)))
      (error "Polys not in same var: 
              MUL-POLY"
             (list p1 p2))))

	;; exercise 2.88

	(define (sub-poly p1 p2)
  (if (same-variable? (variable p1) 
                      (variable p2))
      (make-poly 
       (variable p1)
       (sub-terms (term-list p1)
                  (term-list p2)))
      (error "Polys not in same var: 
              ADD-POLY"
             (list p1 p2))))

	;; adding terms
	(define (arithmetic-terms L1 L2 proc)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) 
               (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 
                   (arithmetic-terms (rest-terms L1) 
                              			 L2 
																		 proc)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 
                   (arithmetic-terms 
                    L1 
                    (rest-terms L2)
										proc)))
                 (else
                  (adjoin-term
                   (make-term 
                    (order t1)
                    (proc (coeff t1) 
                         (coeff t2)))
                   (arithmetic-terms 
                    (rest-terms L1)
                    (rest-terms L2)
										proc))))))))

	(define (add-terms L1 L2)
		(arithmetic-terms L1 L2 add))		
  
		;; Exercise 2.88 (subtracting terms)
	
(define (sub-terms L1 L2)
	(arithmetic-terms L1 L2 sub))

;; multiplying terms
(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms 
       (mul-term-by-all-terms 
        (first-term L1) L2)
       (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term 
          (+ (order t1) (order t2))
          (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms 
          t1 
          (rest-terms L))))))
	
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (mul-poly p1 p2))))

	(put 'sub '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (sub-poly p1 p2))))
	
  (put 'make 'polynomial
       (lambda (var terms) 
         (tag (make-poly var terms))))
	
	'done)

(install-polynomial-package)

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) 
  (null? term-list))
(define (make-term order coeff) 
  (list order coeff))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (term-list p)
	(if (eq? (type-tag p) 'polynomial)
			(cddr p)
			(error "Wrong type:" (type-tag p))))

;; Exercise 2.87: Install =zero? for polynomials in the generic arithmetic package. This will allow adjoin-term to work for polynomials with coefficients that are themselves polynomials.

(define (install-=zero?)
	(define (zero-scheme-number? n) (= n 0))
	(define (zero-rational? n) (= (numer n) 0))
	(define (zero-complex? n) (and (= (real-part n) 0) 
					 											 (= (imag-part n) 0)))
	(define (zero-poly? p) (null? (term-list p)))

	(put '=zero? 'scheme-number zero-scheme-number?)
	(put '=zero? 'rational zero-rational?)
	(put '=zero? 'complex zero-complex?)
	(put '=zero? 'polynomial zero-poly?)
	'done)

(install-=zero?)


	(define (=zero? n)
	(let ((proc (get '=zero? (type-tag n))))
		(if proc
				(proc n)
				(error "Type uknown: =ZERO?" n))))

(define polyA (make-polynomial 'x '((5 1) (4 2) (2 3) (1 -2) (0 -5))))
(define polyB (make-polynomial 'x '((100 1) (2 2) (0 1))))

(define polyC (make-polynomial 'x (list (list 4 (make-rational 2 3))
																				(list 2 (make-complex-from-real-imag 4 1))
																				(list 1 (make-polynomial 'x '((2 3) (0 3)))))))

(define polyD (make-polynomial 'x '((2 3) (0 3))))

	(define (number->poly n var)
		(make-poly-sparse var (list (make-term 0 n))))

	(define (apply-generic op . args)
	;(display "running apply-generic with ") (display args) (newline)
		;(display "trying to apply ") (display op) (display " to ") (display args) (newline)
  (let ((type-tags (map type-tag args)))
		;(display type-tags) (newline)
    (let ((proc (get op type-tags)))
		;	(display proc) (newline)
			;; (display (apply proc (map contents args))) (newline)
      (if proc
          ;(drop 
								(apply proc (map contents args))
					 ;)
          (if (= (length args) 2)
               (let ((a1 (car args))  
                     (a2 (cadr args)))
								 	(cond ((eq? 'polynomial (type-tag a1)) 
												 (apply-generic op a1 (number->poly a2 (variable a1))))
												((eq? 'polynomial (type-tag a2))
												 (apply-generic op (number->poly a1 (variable a2)) a2))
												(else
					                (let ((higher (higher a1 a2)))
														(display "higher is ") (display higher) (newline)
														(cond ((= higher 1) (apply-generic op a1 (raise a2)))
																	((= higher 2) (apply-generic op (raise a1) a2))
																	(else (error "Tried coercion. No method for these types")))))))
							(error 
		              "Didn't tried coercion. No method for these types"
		              (list op type-tags)))))))
																				
;; Exercise 2.89: Define procedures that implement the term-list representation described above as appropriate for dense polynomials.



; dense polynomial representation:
; x^5+2x^4+3x^2−2x−5 is represented as
; (1 2 0 3 -2 -5)
(define (install-dense-polynomial-package)
	
	(define (make-poly variable term-list)
  	(cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
	(define (order p) (-- (length p)))
	(define (coeff p) (car p))

	(put 'make-polynomial 'dense make-poly)
	(put 'variable '(dense) variable)
	(put 'term-list '(dense) term-list)
	(put 'order '(dense) order)
	(put 'coeff '(dense) coeff)
	
(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) 
  (null? term-list))

(define (arithmetic-poly p1 p2 proc)
  (if (same-variable? (variable p1) 
                      (variable p2))
      (make-poly 
       (variable p1)
       (arithmetic-terms (term-list p1)
                  			 (term-list p2) 
												 proc))
      (error "Polys not in same var: 
              ADD-POLY"
             (list p1 p2))))

	(define (arithmetic-terms l1 l2 proc)
		(cond ((empty-termlist? l1) l2)
					((empty-termlist? l2) l1)
					((> (length l1) (length l2)) (cons (first-term l1) 
																						 (arithmetic-terms (rest-terms l1) l2 proc)))
					((> (length l2) (length l1)) (cons (first-term l2)
																						 (arithmetic-terms l1 (rest-term l2) proc)))
					(else (cons (proc (first-term l1) (first-term l2))
											(arithmetic-terms (rest-terms l1) (rest-terms l2) proc)))))

	(define (add-poly p1 p2)
		(arithmetic-poly p1 p2 add))

	(define (sub-poly p1 p2)
		(arithmetic-poly p1 p2 sub))

	(define (add-terms t1 t2)
		(arithmetic-terms t1 t2 add))

(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms
       (mul-term-by-all-terms 
        (first-term L1) L2 (order L1))
       (mul-terms (rest-terms L1) L2))))
	
(define (mul-term-by-all-terms t1 L order)
		(define (itter term poly order result)
			(cond ((and (null? poly) (= order 0)) result)
						((null? poly) (itter term poly (-- order) (append result '(0))))
						(else (itter term 
												 (cdr poly) 
												 order 
												 (append result (list (mul term (coeff poly))))))))
		(itter t1 L order '()))
																										 
	(define my-poly-a (make-poly 'x '(1 2 0 3 -4 5)))
	(define my-poly-b (make-poly 'x '(3 2 1)))

	(display (mul-terms (term-list my-poly-b) (term-list my-poly-a)))
	
	;(put 'make-dense-polynomial 'polynomial make-poly)
	'done)

(define test-poly-a (make-polynomial 'x '((5 1) (4 2) (2 3) (1 -4) (0 5))))
(define test-poly-b (make-polynomial 'x '((2 3) (1 2) (0 1))))

;; Exercise 2.90: Suppose we want to have a polynomial system that is efficient for both sparse and dense polynomials. One way to do this is to allow both kinds of term-list representations in our system. The situation is analogous to the complex-number example of 2.4, where we allowed both rectangular and polar representations. To do this we must distinguish different types of term lists and make the operations on term lists generic. Redesign the polynomial system to implement this generalization. This is a major effort, not a local change.

(define (install-sparse-polynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
	;; (define (first-term l) (car (term-list l)))
	;; (define (rest-terms p)
	;; 	(make-poly (variable p)
	;; 						 (cdr (term-list p))))
	;; (define (order term) (car term))
	;; (define (coeff term) (cadr term))

	(define (tag z) (attach-tag 'sparse z))
	(put 'make-polynomial 'sparse (lambda (v l) (tag (make-poly v l))))
	(put 'variable '(sparse) variable)
	(put 'term-list '(sparse) term-list)
	;; (put 'first-term '(sparse) first-term)
	;; (put 'rest-terms '(sparse) (lambda (p) (tag (rest-terms p))))
	
	;; (put 'order '(sparse) order)
	;; (put 'coeff '(sparse) coeff)

	'done)

(define (install-dense-polynomial-package)
	
	(define (make-poly variable term-list)
  	(cons variable term-list))
  (define (variable p) (car p))
	(define (terms p) (cdr p))
  (define (term-list p) 
		(let ((terms (cdr p)))
			(define (iter result l)
				(cond ((null? l) result)
							((= (car l) 0) (iter result (cdr l)))
							(else (iter (append result (list (list (-- (length l)) (car l))))
										  		(cdr l)))))
				(iter '() terms)))
				
			
	
	;; (define (first-term l) (car (term-list l)))
	;; (define (order term) (car term))
	;; (define (coeff term) (cadr term))
	;; (define (rest-terms p)
	;;  	(make-poly (variable p)
	;;  						 (cdr (terms p))))

	(define (tag z) (attach-tag 'dense z))
	(put 'make-polynomial 'dense (lambda (v l) (tag (make-poly v l))))
	(put 'variable '(dense) variable)
	(put 'term-list '(dense) term-list)
	;; (put 'order '(dense) order)
	;; (put 'coeff '(dense) coeff)
	;; (put 'first-term '(dense) first-term)
	;; (put 'rest-terms '(dense) (lambda (p) (tag (rest-terms p))))

	'done)
(install-sparse-polynomial-package)
(install-dense-polynomial-package)

;(define (order term) (apply-generic 'order term))
;; (define (order term) (car term))
;; (define (coeff term) (apply-generic 'coeff term))
;; (define (coeff term) (cadr term))
;; (define (first-term l) (apply-generic 'first-term l))
(define (term-list p) (apply-generic 'term-list p))
(define (variable p) (apply-generic 'variable p))
;; (define (rest-terms p) (apply-generic 'rest-terms p))

(define (install-polynomial-package)
	(define make-poly (get 'make-polynomial 'sparse))
	(define (empty-termlist? l) (null? l))
	;; (define poly-type car)
	;; (define variable cadr)
	(define (order term) (car term))
	(define (coeff term) (cadr term))
	(define (first-term l) (car l))
	(define (rest-terms l) (cdr l))

	(define (same-termlist? L1 L2)
		(cond ((and (null? L1) (null? L2)) #t)
					((or (null? L1) (null? L2)) #f)
					((=zero? (coeff (first-term L1))) (same-termlist? (cdr L1) L2))
					((=zero? (coeff (first-term L2))) (same-termlist? L1 (cdr L2)))
					((equal? (first-term L1) (first-term L2)) (same-termlist? (cdr L1) (cdr L2)))
					(else #f)))
			

	(define (polynomial->number p)
		(let ((terms (term-list p)))
			(define (iter l)
				(if (empty-termlist? (cdr l))
						(coeff (first-term l))
						(iter (rest-terms l))))
			(iter terms)))
	(put 'project 'polynomial polynomial->number)

	(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
	
 ;; (define (arithmetic-poly p1 p2 term-handler)
 ;;  (if (same-variable? (variable p1) 
 ;;                      (variable p2))
 ;;      (make-poly 
 ;;       (variable p1)
 ;;       (term-handler (term-list p1)
 ;;                  	 (term-list p2)))
 ;;      (error "Polys not in same var: 
 ;;              ARITHMETIC-POLY"
 ;;             (list p1 p2))))

	(define (add-terms L1 L2)
		(cond ((empty-termlist? L1) L2)
					((empty-termlist? L2) L1)
					(else
	         (let ((t1 (first-term L1)) 
	               (t2 (first-term L2)))
	           (cond ((> (order t1) (order t2))
	                  (adjoin-term
	                   t1 
	                   (add-terms (rest-terms L1) L2)))
	                 ((< (order t1) (order t2))
	                  (adjoin-term
	                   t2 
	                   (add-terms 
	                    L1 
	                    (rest-terms L2))))
	                 (else
	                  (adjoin-term
	                   (make-term 
	                    (order t1)
	                    (add (coeff t1) 
	                         (coeff t2)))
	                   (add-terms 
	                    (rest-terms L1)
	                    (rest-terms L2)))))))))

	(define (negation l)
			(define (iter l result)
				(if (null? l) 
						result
						(let ((t (first-term l)))
							(iter (rest-terms l) 
										(append result 
														(list (make-term (order t) 
																			(mul (coeff t) -1))))))))
		(iter l '()))

	(define (sub-terms L1 L2)
		(add-terms L1 (negation L2)))

	(define (add-poly p1 p2)
		(arithmetic-poly p1 p2 add-terms))

		(define (sub-poly p1 p2)
			(arithmetic-poly p1 p2 sub-terms))

		(define (mul-poly p1 p2)
		(arithmetic-poly p1 p2 mul-terms))

;; multiplying terms
(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms 
       (mul-term-by-all-terms 
        (first-term L1) L2)
       (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term 
          (+ (order t1) (order t2))
          (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms 
          t1 
          (rest-terms L))))))

(define (div-poly p1 p2)
  (if (same-variable? (variable p1) 
                      (variable p2))
			(let ((result (div-terms (term-list p1)
                  	 					 (term-list p2))))
				 (list 
	      	(make-poly (variable p1) (car result))
				 	(make-poly (variable p1) (cadr result))))
      (error "Polys not in same var: 
              DIV-POLY"
             (list p1 p2))))
	
(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) 
            (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) 
                              (coeff t2)))
                  (new-o (- (order t1) 
                            (order t2))))
              (let ((rest-of-result
											 (div-terms
												 (sub-terms
													L1
												 	(mul-term-by-all-terms (list new-o new-c) L2)) 
												L2)))
								(list (adjoin-term (make-term new-o new-c)
																	 (car rest-of-result))
											(cadr rest-of-result))))))))

	;; Exercise 2.92: By imposing an ordering on variables, extend the polynomial package so that addition and multiplication of polynomials works for polynomials in different variables. (This is not easy!)

	;; (add (polynomial sparse x (1 1)) (polynomial sparse y (1 1)))
	;; ]=> (polynomial sparse x (1 1) (0 (polynomial sparse y (1 1))))

	;; One possibility is to convert one polynomial to the type of the other by expanding and rearranging terms so that both polynomials have the same principal variable. 

	;; y^2.x + y.2x + x^2
	;; (polynomial sparse y (2 (polynomial sparse x (1 1))) 
	;;  										(1 (polynomial sparse x (1 2)))
	;; 										  (0 (polynomial sparse x (2 1))))

	;; ((2 (1 1)) (1 (1 2)) (0 (2 1)))
  ;; switching it up:
	;; ((1 (2 1)) (1 (1 2)) (2 1)) ;; x.y^2 + x.2y + x^2 ;; if 0 (and poly?) just use the poly, else maintain as it is (e.g. (0 2) would remain (0 2))
	;; reorder
	;; ((2 1) (1 (1 2)) (1 (2 1)))
	;; unify same powers
	;; ((2 1) (1 (1 2) (2 1)))
	;; reorder internally like before (recursive?)
	;; ((2 1) (1 (2 1) (1 2)))

	;; x^2 + x.(y^2 + 2y) + y^2
	;; (polynomial sparse x (2 1)
	;;  										(1 (polynomial sparse y (2 1) (1 2)))
	;; 										  (0 (polynomial sparse y (2 1))))
	

	;; from https://gitlab.com/barry.allison/wizard-book-solutions/blob/master/ch02/ex2.92.rkt

	(define (var-order>? v1 v2)
		(string>? (symbol->string v1)
							(symbol->string v2)))

	(define (coerce-poly psrc ptarget)
		(let ((coerce-var (variable ptarget))
					(zeroth-term (make-term 0 (tag psrc))))
			(make-poly coerce-var (list zeroth-term))))

(define (arithmetic-poly p1 p2 term-handler)
	(let ((v1 (variable p1))
				(v2 (variable p2)))
		(cond ((same-variable? v1 v2)
					 (make-poly 
						 (variable p1)
						 (term-handler (term-list p1)
													 (term-list p2))))
				((var-order>? v1 v2)
				 (arithmetic-poly (coerce-poly p1 p2) p2 term-handler))
				(else
				 (arithmetic-poly p1 (coerce-poly p2 p1) term-handler)))))

		
	(define (number->poly n var)
		(make-poly-sparse var (list (make-term (0 n)))))

;; Exercise 2.94

	(define (gcd-terms a b)
  (if (empty-termlist? b)
			(div-terms-by-int a (gcd-of-termlist a)) ;; new implementatino
      ;; a ;; original implementation
      (gcd-terms b (pseudoremainder-terms a b))))

	;; (trace gcd-terms)	

	;; Exercise 2.96
	
	(define (gcd-of-termlist L)
		(define (gcd-of-terms t1 t2)
			(gcd (coeff t1) (coeff t2)))
		(define (iter result L)
			(if (= (length L) 1)
					result
					(iter (gcd-of-terms (car L) (cadr L)) (cdr L))))
		(iter (coeff (car L)) L))

	(define (div-terms-by-int L int)
		(if (null? L)
				'()
				(let ((current-term (car L)))
					(cons (make-term (order current-term) 
													 (/ (coeff current-term) int))
								(div-terms-by-int (cdr L) int)))))
	
	(define (remainder-terms a b)
		(cadr (div-terms a b)))

	(define (pseudoremainder-terms p q)
		(let ((o1 (caar p))
					(o2 (caar q))
					(c  (cadar q)))
			(let ((integerizing-factor (expt c (+ 1 (- o1 o2)))))
				(cadr (div-terms (mul-terms (list (list 0 integerizing-factor))
																		p) 
												 q)))))

		
	(define (gcd-poly p1 p2)
		(arithmetic-poly p1 p2 gcd-terms))

	;; Exercise 2.97

	;; (define (reduce-terms n d)
	;; 	;; Compute the GCD of the numerator and denominator, using the version of gcd-terms from Exercise 2.96
	;; 	(let ((gcd-of-n-d (gcd-terms n d)))
	;; 				 ;; (display "gcd-of-n-d")(show gcd-of-n-d) (newline)
	;; 	;; When you obtain the GCD, multiply both numerator and denominator by the same integerizing factor before dividing through by the GCD, so that division by the GCD will not introduce any noninteger coefficients. As the factor you can use the leading coefficient of the GCD raised to the power 1+O1−O2, where O2 is the order of the GCD and O1 is the maximum of the orders of the numerator and denominator. This will ensure that dividing the numerator and denominator by the GCD will not introduce any fractions.
	;; 				 (let ((c (coeff (car gcd-of-n-d)))
	;; 				 			 (o1 (max (caar n) (caar d)))
	;; 				 			 (o2 (order (car gcd-of-n-d))))
	;; 					 ;; (display "c, o1, o2") (show c) (show o1) (show o2) (newline)
	;; 					 (let ((integerizing-factor (expt c (+ 1 (- o1 o2)))))
	;; 						 ;; (display "int-factor") (show integerizing-factor) (newline)
	;; 						 (let ((factor-to-multiply (list (make-term 0 integerizing-factor))))
	;; 							 ;; (display "factor-to-multiply") (show factor-to-multiply) (newline)
	;; 							 (let ((new-n (car (div-terms (mul-terms n factor-to-multiply) gcd-of-n-d)))
	;; 				 						 (new-d (car (div-terms (mul-terms d factor-to-multiply) gcd-of-n-d))))
	;; 								 ;; (display "new-n,new-d") (show new-n) (show new-d) (newline)
	;; 	;; The result of this operation will be a numerator and denominator with integer coefficients. The coefficients will normally be very large because of all of the integerizing factors, so the last step is to remove the redundant factors by computing the (integer) greatest common divisor of all the coefficients of the numerator and the denominator and dividing through by this factor.
	;; 									 (let ((gcd-of-new-factors (gcd-terms new-n new-d)))
	;; 										 ;; (display "gcd-of-new-factors") (show gcd-of-new-factors) (newline)
	;; 										 (list (car (div-terms new-n gcd-of-new-factors))
	;; 													 (car (div-terms new-d gcd-of-new-factors))))))))))

	(define (reduce-terms n d)
		(let* ((gcd-of-n-d (gcd-terms n d))
					 (c (coeff (car gcd-of-n-d)))
					 (o1 (max (caar n) (caar d)))
					 (o2 (order (car gcd-of-n-d)))
					 (integerizing-factor (expt c (+ 1 (- o1 o2))))
					 (factor-to-multiply (list (make-term 0 integerizing-factor)))
					 (new-n (car (div-terms (mul-terms n factor-to-multiply) gcd-of-n-d)))
					 (new-d (car (div-terms (mul-terms d factor-to-multiply) gcd-of-n-d)))
					 (gcd-of-new-factors (gcd-terms new-n new-d))
					 (nn (car (div-terms new-n gcd-of-new-factors)))
					 (dd (car (div-terms new-d gcd-of-new-factors))))
							 (list new-n new-d)))

	;; (define (reduce-poly p1 p2)
	;; 	(arithmetic-poly p1 p2 reduce-terms))

	(define (reduce-poly p1 p2)
	  (if (same-variable? (variable p1) 
	                      (variable p2))
				(let ((result (reduce-terms (term-list p1)
	                  	 					 	 (term-list p2))))
		      	(list (make-polynomial (variable p1) (car result))
					 				(make-polynomial (variable p1) (cadr result))))
	      (error "Polys not in same var: 
	              REDUCE-POLY"
	             (list p1 p2))))
			
			
	
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'polynomial z))
	(put 'make-poly 'polynomial (lambda (v l) (tag (make-poly v l))))
	(put 'make-poly-sparse 'polynomial 
			 (lambda (v l) (tag ((get 'make-polynomial 'sparse) v l))))
	(put 'make-poly-dense 'polynomial 
			 (lambda (v l) (tag ((get 'make-polynomial 'dense) v l))))

	(put 'first-term	'(polynomial) first-term)
	(put 'term-list		'(polynomial) term-list)
	(put 'order			  '(polynomial) order)
	(put 'coeff				'(polynomial) coeff)
	(put 'first-term	'(polynomial) first-term)
	(put 'term-list		'(polynomial) term-list)
	(put 'variable		'(polynomial) variable)
	(put 'rest-terms	'(polynomial) rest-terms)
	
	(put 'add '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (mul-poly p1 p2))))

	(put 'sub '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (sub-poly p1 p2))))

	(put 'div '(polynomial polynomial)
       (lambda (p1 p2) 
				 (let ((result (div-poly p1 p2)))
					 (list (tag (car result)) (tag (cadr result))))))

	(put 'greatest-common-divisor '(polynomial polynomial) 
			 (lambda (p1 p2)
				 (tag (gcd-poly p1 p2))))

	(put 'reduce '(polynomial polynomial) reduce-poly)

	;; TEMP-PUTs, for testing
	;; (put 'add-poly '(polynomial polynomial) add-poly)
	;; (put 'add-terms '(polynomial polynomial) add-terms)
	;; (put 'arithmetic-terms '(polynomial polynomial) arithmetic-terms)

	;; (put 'poly-type '(polynomial) poly-type)
	;; (put 'variable '(polynomial) variable)
	
	;; (put 'div-terms 'polynomial div-terms)
	;; (put 'negation 'polynomial negation)
	;; (put 'reduce-terms 'polynomial reduce-terms)
	
	'done)

(install-polynomial-package)

;; (define (variable p) (apply-generic 'variable p))
;; (define (poly-type p) (apply-generic 'poly-type p))

(define make-poly-sparse (get 'make-poly-sparse 'polynomial))
(define make-poly-dense (get 'make-poly-dense 'polynomial))
(define make-poly (get 'make-poly 'polynomial))

(define dense-poly-a (make-poly-dense 'x '(1 2 0 3 -4 5)))
(define dense-poly-b (make-poly-dense 'x '(3 2 1)))
(define sparse-poly-a (make-poly-sparse 'x '((5 1) (4 2) (2 3) (1 -4) (0 5))))
(define sparse-poly-b (make-poly-sparse 'x '((2 3) (1 2) (0 1))))


;; (display (add dense-poly-a dense-poly-b)) (newline)
;; (display (add sparse-poly-a dense-poly-b)) (newline)
;; (display (add dense-poly-a sparse-poly-b)) (newline)
;; (display (add dense-poly-a sparse-poly-a)) (newline)

;; (display (sub dense-poly-a dense-poly-b)) (newline)
;; (display (sub sparse-poly-a dense-poly-b)) (newline)
;; (display (sub dense-poly-a sparse-poly-b)) (newline)
;; (display (sub dense-poly-a sparse-poly-a)) (newline)

;; (display (mul dense-poly-a dense-poly-b)) (newline)
;; (display (mul sparse-poly-a dense-poly-b)) (newline)
;; (display (mul dense-poly-a sparse-poly-b)) (newline)
;; (display (mul dense-poly-a sparse-poly-a)) (newline)

(define div-poly-a (make-poly-sparse 'x '((5 1) (0 -1))))
(define div-poly-b (make-poly-sparse 'x '((2 1) (0 -1))))
(define l1 (term-list div-poly-a))
(define l2 (term-list div-poly-b))
;(define div-terms-result ((get 'div-terms 'polynomial) l1 l2))
;; (display (sub div-poly-a div-poly-b))(newline)
;; (display (div div-poly-a div-poly-b))(newline)
;; (display ((get 'negation 'polynomial) div-poly-a))(newline)
;; (display (sub dense-poly-a sparse-poly-b)) (newline)
;; (display (div div-poly-a div-poly-b)) (newline)


(define px1 (make-poly-sparse 'x '((4 2) (2 1) (0 5))))
(define px2 (make-poly-sparse 'x '((3 7) (1 3.5))))
(define py1 (make-poly-sparse 'y '((5 1) (4 3) (2 4.7) (0 -8.1))))
(define py2 (make-poly-sparse 'y '((4 2.3) (3 2.) (2 4) (1 20))))
(define pxy1 (make-poly-sparse 'y (list (list 3 px1)
                                              (list 2 px2)
                                              (list 1 0.4)
                                              (list 0 px1))))
(define pxy2 (make-poly-sparse 'y (list (list 4 px2)
                                              (list 3 2.36)
                                              (list 1 px1))))

(define pxy3 (make-poly-sparse 'y (list (list 4 px2)
                                              (list 2 px1)
                                              (list 1 px1))))

(define (show x) (newline) (display x))

;; (show (add px1 py2))
;;Value: (polynomial y (4 2.3) (3 2.) (2 4) (1 20) (0 (polynomial x (4 2) (2 1) (0 5))))
;
;; (show (add pxy1 pxy1))
;; (show (add px1 py2))
;;Value: (polynomial y (5 1) (4 3) (2 4.7) (0 (polynomial x (3 7) (1 3.5) (0 -8.1))))
;
;(show (add pxy3 pxy2))
;;Value: (polynomial y (4 (polynomial x (3 14) (1 7.)))
;;                     (3 2.36)
;;                     (2 (polynomial x (4 2) (2 1) (0 5)))
;;                     (1 (polynomial x (4 4) (2 2) (0 10))))
;
;(show (sub px2 py1))
;;Value: (polynomial y (5 -1) (4 -3) (2 -4.7) (0 (polynomial x (3 7) (1 3.5) (0 8.1))))
;
;(show (sub pxy2 px2))
;;Value: (polynomial y (4 (polynomial x (3 7) (1 3.5)))
;;                     (3 2.36)
;;                     (1 (polynomial x (4 2) (2 1) (0 5)))
;;                     (0 (polynomial x (3 -7) (1 -3.5))))
;
;(show (mul py2 px2))
;;Value: (polynomial y (4 (polynomial x (3 16.099999999999998) (1 8.049999999999999)))
;;                     (3 (polynomial x (3 14.) (1 7.)))
;;                     (2 (polynomial x (3 28) (1 14.)))
;;                     (1 (polynomial x (3 140) (1 70.))))
;
;(show (mul pxy1 px2))
;;Value: (polynomial y (3 (polynomial x (7 14) (5 14.) (3 38.5) (1 17.5)))
;;                     (2 (polynomial x (6 49) (4 49.) (2 12.25)))
;;                     (1 (polynomial x (3 2.8000000000000003) (1 1.4000000000000001)))
;;                     (0 (polynomial x (7 14) (5 14.) (3 38.5) (1 17.5))))
;
;(show (mul pxy2 pxy3))
;;Value: (polynomial y (8 (polynomial x (6 49) (4 49.) (2 12.25)))
;;                     (7 (polynomial x (3 16.52) (1 8.26)))
;;                     (6 (polynomial x (7 14) (5 14.) (3 38.5) (1 17.5)))
;;                     (5 (polynomial x (7 28) (5 28.) (4 4.72) (3 77.) (2 2.36) (1 35.) (0 11.799999999999999)))
;;                     (4 (polynomial x (4 4.72) (2 2.36) (0 11.799999999999999)))
;;                     (3 (polynomial x (8 4) (6 4) (4 21) (2 10) (0 25)))
;;                     (2 (polynomial x (8 4) (6 4) (4 21) (2 10) (0 25))))
;
;(show (mul (mul px1 py1) pxy2))
;;Value: (polynomial y (9 (polynomial x (7 14) (5 14.) (3 38.5) (1 17.5)))
;;                     (8 (polynomial x (7 42) (5 42.) (4 4.72) (3 115.5) (2 2.36) (1 52.5) (0 11.799999999999999)))
;;                     (7 (polynomial x (4 14.16) (2 7.08) (0 35.4)))
;;                     (6 (polynomial x (8 4) (7 65.8) (6 4) (5 65.8) (4 21) (3 180.95) (2 10) (1 82.25) (0 25)))
;;                     (5 (polynomial x (8 12) (6 12) (4 85.184) (2 41.092) (0 130.45999999999998)))
;;                     (4 (polynomial x (7 -113.39999999999999) (5 -113.39999999999999) (3 -311.85) (1 -141.75)))
;;                     (3 (polynomial x (8 18.8) (6 18.8) (4 60.468) (2 27.884) (0 21.92)))
;;                     (1 (polynomial x (8 -32.4) (6 -32.4) (4 -170.1) (2 -81.) (0 -202.5))))

;; Exercise 2.93

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cadr x))

	;; Exercise 2.97:
	
	(define (reduce-integers n d)
	  (let ((g (gcd n d)))
	    (list (/ n g) (/ d g))))
	
   (define (make-rat n d)
       (reduce n d))
	
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                 	 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                 	 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
	 (put 'reduce '(scheme-number scheme-number) reduce-integers)

	 'done)

(install-rational-package)
(define make-polynomial make-poly)

;; (define (make-rational n d)
;;   ((get 'make 'rational) n d))



;; (define p1 (make-poly 'x '((2 1) (0 1))))
;; (define p2 (make-poly 'x '((3 1) (0 1))))
;; (define rf (make-rational p2 p1))

;; Exercise 2.94

(define (install-gcd-package)
	(define (gcd a b)
	  (if (= b 0)
	      a
	      (gcd b (remainder a b))))
	
		(put 'greatest-common-divisor '(scheme-number scheme-number) gcd)
		(put 'greatest-common-divisor '(rational rational) gcd)
		(put 'greatest-common-divisor '(complex complex) gcd)
	
	'done)

(install-gcd-package)

(define (greatest-common-divisor a b) (apply-generic 'greatest-common-divisor a b))

(define p1 
  (make-polynomial 
   'x '((4 1) (3 -1) (2 -2) (1 2))))

(define p2 
  (make-polynomial 
   'x '((3 1) (1 -1))))

;; Exercise 2.95

(define p1
	(make-poly 'x '((2 1) (1 -2) (0 1))))

(define p2
	(make-poly 'x '((2 11) (0 7))))

(define p3
	(make-poly 'x '((1 13) (0 6))))

(define q1 (mul p1 p2))
(define q2 (mul p1 p3))

(define t1 (cdddr q1))
(define t2 (cdddr q2))

;; Exercise 2.97:

(define (reduce x y) (apply-generic 'reduce x y))

(define p1 
  (make-polynomial 'x '((1 1) (0 1))))
(define p2 
  (make-polynomial 'x '((3 1) (0 -1))))
(define p3 
  (make-polynomial 'x '((1 1))))
(define p4 
  (make-polynomial 'x '((2 1) (0 -1))))
(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p3 p4))
(show (add rf1 rf2))
