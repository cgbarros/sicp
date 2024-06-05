(load "allcode/ch3support.scm")
(load "allcode/ch3.scm")

(define the-agenda (make-agenda))

;; this is needed for the or-gate
;; it's just a variation of logical-and
;; from ch3support.scm
(define (logical-or x y)
	(if (or (= x 1) (= y 1))
			1
			0))

;; arbitrary delays
(define or-gate-delay 1)
(define and-gate-delay 1)
(define inverter-delay 1)

;; exercise 3.28 - build an or-gate
(define (or-gate a1 a2 output)
	(define (or-action-procedure)
		(let ((new-value
						(logical-or (get-signal a1) (get-signal a2))))
			(after-delay
				or-gate-delay
				(lambda () (set-signal! output new-value)))))
			 ;; (set-signal! output new-value))))
	(add-action! a1 or-action-procedure)
	(add-action! a2 or-action-procedure)
	'ok)

;;;; tests
;; (define a (make-wire))
;; (define b (make-wire))
;; (set-signal! a 1)
;; (set-signal! b 0)
;; (define c (make-wire))

;; (or-gate a b c)
;; (display (get-signal c))
;;;; it only works if we propagate
;; (propagate)
;; (display (get-signal c))

;; exercise 3.29 - build an or-gate using and-gates and inverters

;; morgan's law: a or b = (not (and (not a) (not b)))

(define (new-or-gate a b output)
	(let ((a-out (make-wire))
				(b-out (make-wire))
				(and-out (make-wire)))
		(inverter a a-out)
		(inverter b b-out)
		(and-gate a-out b-out and-out)
		(inverter and-out output)))

;; delays:
;; 2 inverter delays + 1 and-gate delay + 1 inverter delay

;; ;; tests
;; (define new-a (make-wire))
;; (define new-b (make-wire))
;; (define new-o (make-wire))

;; (set-signal! new-a 0)
;; (set-signal! new-b 0)
;; (new-or-gate new-a new-b new-o)
;; (propagate)
;; (display (get-signal new-o))
;; (newline)

;; (set-signal! new-a 1)
;; (set-signal! new-b 0)
;; (new-or-gate new-a new-b new-o)
;; (propagate)
;; (display (get-signal new-o))
;; (newline)

;; (set-signal! new-a 0)
;; (set-signal! new-b 1)
;; (new-or-gate new-a new-b new-o)
;; (propagate)
;; (display (get-signal new-o))
;; (newline)

;; (set-signal! new-a 1)
;; (set-signal! new-b 1)
;; (new-or-gate new-a new-b new-o)
;; (propagate)
;; (display (get-signal new-o))
;; (newline)

;; (define (nor-gate a b output)
;; 	(let ((a-out (make-wire))
;; 				(b-out (make-wire)))
;; 		(inverter a a-out)
;; 		(inverter b b-out)
;; 		(and-gate a-out b-out output)))

;; exercise 3.30 - build a ripple-carry adder

;; FA needs: a, b, c-in, sum, c-out - the last one we have to create.

(define (ripple-carry-adder a-list b-list s-list c-in c-out)
	  (if (null? a-list)
				(add-action! c-in (lambda () (set-signal! c-out (get-signal c-in))))	
			  (let ((c-n (make-wire)))
					(full-adder (car a-list)
											(car b-list) 
											c-in
											(car s-list) 
											c-n)
					(ripple-carry-adder (cdr a-list) 
															(cdr b-list) 
															(cdr s-list)
															c-n
															c-out))))

(define (decimal-to-binary-list n)
	(define (helper n list)
		(cond ((= n 0) list)
					((= n 1) (cons 1 list))
					(else (helper (quotient n 2) (cons (remainder n 2) list)))))
	(helper n '()))

(define (set-binary-number! wires number)
	(define (helper wires signal-list)
		(cond ((null? wires) 'ok)
					((null? signal-list)
					 (begin
					 (set-signal! (car wires) 0))
					 (helper (cdr wires) '()))
					(else 		 
						(let ((signal (car signal-list)))
							(set-signal! (car wires) signal)
							(helper (cdr wires) (cdr signal-list))))))
		(helper wires (reverse (decimal-to-binary-list number))))
		

(define (get-binary-number wires)
	(define (helper wires position number)
		(if (null? wires)
				number
				(let ((bit (get-signal (car wires))))
					(helper (cdr wires) 
									(1+ position) 
									(+ number (* (expt 2 position) bit))))))
	(helper wires 0 0))

(define (get-signals wires)
	(map get-signal wires))

;; tests
;; (define a-list (list (make-wire) (make-wire) (make-wire) (make-wire)))
;; (define b-list (list (make-wire) (make-wire) (make-wire) (make-wire)))
;; (define s-list (list (make-wire) (make-wire) (make-wire) (make-wire)))
;; (define c-in (make-wire))
;; (define c-out (make-wire))


;; (set-binary-number! a-list 8)
;; (set-binary-number! b-list 5)
;; (set-signal! c-in 0)

;; (display "a-list: ")
;; (display (get-signals a-list))
;; (newline)
;; (display "a-list number: ")
;; (display (get-binary-number a-list))
;; (newline)
;; (display "b-list: ")
;; (display (get-signals b-list))
;; (newline)
;; (display "b-list number: ")
;; (display (get-binary-number b-list))
;; (newline)

;; (ripple-carry-adder a-list b-list s-list c-in c-out)
;; (propagate)

;; (display "s-list: ")
;; (display (get-signals s-list))
;; (newline)
;; (display "s-list number: ")
;; (display (get-binary-number s-list))
;; (newline)
;; (display "carry: ")
;; (display (get-signal c-out))
;; (newline)

;; ;; Test case 1: Adding 15 (1111) and 1 (0001)
;; (set-binary-number! a-list 15)
;; (set-binary-number! b-list 1)
;; (set-signal! c-in 0)

;; (ripple-carry-adder a-list b-list s-list c-in c-out)
;; (propagate)

;; (display "Test case 1 - a-list: ")
;; (display (get-signals a-list))
;; (newline)
;; (display "Test case 1 - b-list: ")
;; (display (get-signals b-list))
;; (newline)
;; (display "Test case 1 - s-list: ")
;; (display (get-signals s-list))
;; (newline)
;; (display "Test case 1 - s-list number: ")
;; (display (get-binary-number s-list))
;; (newline)
;; (display "Test case 1 - carry: ")
;; (display (get-signal c-out))
;; (newline)

;; ;; Test case 2: Adding 0 (0000) and 0 (0000)
;; (set-binary-number! a-list 0)
;; (set-binary-number! b-list 0)
;; (set-signal! c-in 0)

;; (ripple-carry-adder a-list b-list s-list c-in c-out)
;; (propagate)

;; (display "Test case 2 - a-list: ")
;; (display (get-signals a-list))
;; (newline)
;; (display "Test case 2 - b-list: ")
;; (display (get-signals b-list))
;; (newline)
;; (display "Test case 2 - s-list: ")
;; (display (get-signals s-list))
;; (newline)
;; (display "Test case 2 - s-list number: ")
;; (display (get-binary-number s-list))
;; (newline)
;; (display "Test case 2 - carry: ")
;; (display (get-signal c-out))
;; (newline)


; each half-adder needs to wait for 1 or-gate + 2 and-gates + 1 inverter delay
; the full adder needs two half-adders + 1 or-gate, i.e. 3 or-gates + 4 and-gates + 2 inverter delay
; the ripple-carry will have a delay of n * (3 ogd + 4agd + 2id)