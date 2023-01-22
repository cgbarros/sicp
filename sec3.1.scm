(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance 
                       (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds")))

(define w1 (make-withdraw 100))
(define w2 (make-withdraw 100))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: 
                 MAKE-ACCOUNT" m))))
  dispatch)

(define ac1 (make-account 100))

;; Exercise 3.1: An accumulator is a procedure that is called repeatedly with a single numeric argument and accumulates its arguments into a sum. Each time it is called, it returns the currently accumulated sum. Write a procedure make-accumulator that generates accumulators, each maintaining an independent sum. The input to make-accumulator should specify the initial value of the sum; for example

;; (define A (make-accumulator 5))

;; (A 10)
;; 15

;; (A 10)
;; 25

(define (make-accumulator n)
	(let ((current-value n))
		(lambda (addend)
			(begin (set! current-value (+ current-value addend))
						 current-value))))

;; Exercise 3.2: In software-testing applications, it is useful to be able to count the number of times a given procedure is called during the course of a computation. Write a procedure make-monitored that takes as input a procedure, f, that itself takes one input. The result returned by make-monitored is a third procedure, say mf, that keeps track of the number of times it has been called by maintaining an internal counter. If the input to mf is the special symbol how-many-calls?, then mf returns the value of the counter. If the input is the special symbol reset-count, then mf resets the counter to zero. For any other input, mf returns the result of calling f on that input and increments the counter. For instance, we could make a monitored version of the sqrt procedure:

;; (define s (make-monitored sqrt))

;; (s 100)
;; 10

;; (s 'how-many-calls?)
;; 1

(define (make-monitored proc)
	(let ((counter 0))
		(define (count) (set! counter (+ counter 1)))
		(define (reset)
			(begin (set! counter 0)
						 counter))
		(define (dispatch m)
			(cond ((eq? m 'how-many-calls?) counter)
						((eq? m 'reset-count) (reset))
						(else (count) (proc m))))
						 dispatch))

(define s (make-monitored sqrt))

;; Exercise 3.3: Modify the make-account procedure so that it creates password-protected accounts. That is, make-account should take a symbol as an additional argument, as in

;; (define acc 
;;   (make-account 100 'secret-password))
;; The resulting account object should process a request only if it is accompanied by the password with which the account was created, and should otherwise return a complaint:

;; ((acc 'secret-password 'withdraw) 40)
;; 60

;; ((acc 'some-other-password 'deposit) 50)
;; "Incorrect password"

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
		(if (eq? p 'secret-password)
				(cond ((eq? m 'withdraw) withdraw)
          		((eq? m 'deposit) deposit)
		          (else (error "Unknown request: 
		                 MAKE-ACCOUNT" m)))
				(lambda (m) "Incorrect password")))
  dispatch)

(define myAcc (make-account 100))

;; Exercise 3.4: Modify the make-account procedure of Exercise 3.3 by adding another local state variable so that, if an account is accessed more than seven consecutive times with an incorrect password, it invokes the procedure call-the-cops.

(define (make-account balance)
	(let ((counter 0))
	  (define (withdraw amount)
	    (if (>= balance amount)
	        (begin (set! balance 
	                     (- balance amount))
	               balance)
	        "Insufficient funds"))
	  (define (deposit amount)
	    (set! balance (+ balance amount))
	    balance)
		(define (incorrect-password)
			(if (< counter 7)
					(begin (set! counter (++ counter))
								  "Incorrect password")
					(call-the-cops)))
	  (define (dispatch p m)
			(if (eq? p 'secret-password)
					(begin (set! counter 0)
								 (cond ((eq? m 'withdraw) withdraw)
								  	   ((eq? m 'deposit) deposit)
											 (else (error "Unknown request: 
											 			 MAKE-ACCOUNT" m))))
					(lambda (m) (incorrect-password))))
	  dispatch))

(define (call-the-cops) "We're calling the cops!")

(define myAcc (make-account 100))

;; 3.1.2 The Benefits of Introducing Assignment

(define random-init 1)

(define rand
  (let ((x random-init))
    (lambda () (set! x (rand-update x)) x)))

;; tmp definition for tests
(define (rand) (+ 1 (random 1000000000)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials 
                          cesaro-test))))
(define (cesaro-test)
   (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) 
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) 
                 trials-passed))))
  (iter trials 0))

;; Exercise 3.5

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (point-inside-circle? point circle)
  (<= (+ (square (- (xcor (center circle)) (xcor point)))
	 (square (- (ycor (center circle)) (ycor point))))
      (square (radius circle))))

(define make-point cons)
(define xcor car)
(define ycor cdr)
(define (make-circle center radius) (cons center radius))
(define center car)
(define radius cdr)
(define make-rectangle cons)
(define lower-corner car)
(define higher-corner cdr)
(define (area-rec rectangle)
  (* (- (xcor (higher-corner rectangle))
	(xcor (lower-corner rectangle)))
     (- (ycor (higher-corner rectangle))
	(ycor (lower-corner rectangle)))))

(define unit-circle (make-circle (make-point 0 0) 1))
(define inside-point (make-point 0.5 0.5))
(define outside-point (make-point 2 3))
;; (display (point-inside-circle? inside-point unit-circle)) (newline)
;; (display (point-inside-circle? outside-point unit-circle)) (newline)


(define (random-inside-circle? rect circle)
  (let ((xlow (xcor (lower-corner rect)))
	(xhigh (xcor (higher-corner rect)))
	(ylow (ycor (lower-corner rect)))
	(yhigh (ycor (higher-corner rect))))
    (let ((random-point (make-point (random-in-range xlow xhigh)
				    (random-in-range ylow yhigh))))
  (point-inside-circle? random-point circle))))

(define random-inside-unit-circle?
  (lambda (rect)
    (random-inside-circle? rect unit-circle)))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (let ((rect (make-rectangle (make-point x1 y1)
			      (make-point x2 y2))))
      (* (monte-carlo trials (lambda () (P rect))) (area-rec rect))))

(define (integral-unit-circle trials)
  (estimate-integral random-inside-unit-circle? -1.0 1.0 -1.0 1.0 trials))

;; (display (integral-unit-circle 1000000)) (newline)

;; Exercise 3.6

;;original rand
(define rand
    (let ((x random-init))
          (lambda () (set! x (rand-update x)) x)))

(define random-init 1)
(define rand-update ++)

(define rand
	(let ((x random-init))
		(define (generate) 
			(set! x (rand-update x))
			x)
		(define (reset new-value)
			(set! x new-value)
			x)
		(define (dispatch m)
			(cond ((eq? m 'generate) (generate))
						((eq? m 'reset) reset)))
		dispatch))

;; Exercise 3.7: Consider the bank account objects created by make-account, with the password modification described in Exercise 3.3. Suppose that our banking system requires the ability to make joint accounts. Define a procedure make-joint that accomplishes this. Make-joint should take three arguments. The first is a password-protected account. The second argument must match the password with which the account was defined in order for the make-joint operation to proceed. The third argument is a new password. Make-joint is to create an additional access to the original account using the new password. For example, if peter-acc is a bank account with password open-sesame, then

;; (define paul-acc
;;   (make-joint peter-acc 
;;               'open-sesame 
;;               'rosebud))
;; will allow one to make transactions on peter-acc using the name paul-acc and the password rosebud. You may wish to modify your solution to Exercise 3.3 to accommodate this new feature.

(define (make-account password balance)
	(let ((counter 0))
	  (define (withdraw amount)
	    (if (>= balance amount)
	        (begin (set! balance 
	                     (- balance amount))
	               balance)
	        "Insufficient funds"))
	  (define (deposit amount)
	    (set! balance (+ balance amount))
	    balance)
		(define (incorrect-password)
			(if (< counter 7)
					(begin (set! counter (++ counter))
								  (error "Incorrect password"))
					(call-the-cops)))
	  (define (dispatch p m)
			(if (eq? p password)
					(begin (set! counter 0)
								 (cond ((eq? m 'withdraw) withdraw)
								  	   ((eq? m 'deposit) deposit)
											 ((eq? m 'verify) #t)
											 (else (error "Unknown request: 
											 			 MAKE-ACCOUNT" m))))
;					(lambda (m) (incorrect-password))))
					(incorrect-password)))
	  dispatch))

(define (make-joint account-name oldpass newpass)
	(let ((joint-try (account-name oldpass 'verify)))
		(if joint-try
				(lambda (pwd m)
					(if (eq? pwd newpass)
							(account-name oldpass m)
							(error "Invalid access to joint account")))
				'())))

;; (define peter-acc (make-account 'open-sesame 100))
;; (display ((peter-acc 'open-sesame 'withdraw) 10))
;; (newline)
;; (display (peter-acc 'open-sesame 'verify))
;; (newline)

;; (define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
;; (display ((paul-acc 'rosebud 'withdraw) 10))
;; (newline)
;; (display ((paul-acc 'wrong-pass 'withdraw) 10))
;; (newline)
;; (display ((peter-acc 'rosebud 'withdraw) 10))

;; Exercise 3.8: When we defined the evaluation model in 1.1.3, we said that the first step in evaluating an expression is to evaluate its subexpressions. But we never specified the order in which the subexpressions should be evaluated (e.g., left to right or right to left). When we introduce assignment, the order in which the arguments to a procedure are evaluated can make a difference to the result. Define a simple procedure f such that evaluating

;; (+ (f 0) (f 1))

;; will return 0 if the arguments to + are evaluated from left to right but will return 1 if the arguments are evaluated from right to left.

;; after many tries

(define f
	(let ((x 0))
		(lambda (y)
			(if (= y 0)
					x
					(begin (set! x y)
								 (- x y))))))

;; Exercise 3.12

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

;; (show (cdr x))
;; (b)

(define w (append! x y))

;; (show (cdr x))
;; (b c d)

;; Exercise 3.13: Consider the following make-cycle procedure, which uses the last-pair procedure defined in Exercise 3.12:

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
;; Draw a box-and-pointer diagram that shows the structure z created by

(define z (make-cycle (list 'a 'b 'c)))

;; What happens if we try to compute (last-pair z)?

;; infinite loop

;; Exercise 3.14: The following procedure is quite useful, although obscure:

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;; Loop uses the “temporary” variable temp to hold the old value of the cdr of x, since the set-cdr! on the next line destroys the cdr. Explain what mystery does in general. Suppose v is defined by (define v (list 'a 'b 'c 'd)). Draw the box-and-pointer diagram that represents the list to which v is bound. Suppose that we now evaluate (define w (mystery v)). Draw box-and-pointer diagrams that show the structures v and w after evaluating this expression. What would be printed as the values of v and w?

;; mystery reverses the list, but destroys the links of the original list
;; w would return (b c d a)
;; v would return (a)

(define v (list 'a 'b 'c 'd))
(define w (mystery v))

;; (show w)
;; (d c b a)
;; (show v)
;; (a)