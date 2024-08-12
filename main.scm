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

;; ;; arbitrary delays
;; (define or-gate-delay 1)
;; (define and-gate-delay 1)
;; (define inverter-delay 1)

;; exercise 3.28 - build an or-gate
(define (or-gate a1 a2 output)
	(define (or-action-procedure)
		(let ((new-value
						(logical-or (get-signal a1) (get-signal a2))))
			(after-delay
				or-gate-delay
				(lambda () (set-signal! output new-value)))))
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

;; Exercise 3.31:e internal procedure accept-action-procedure! defined in make-wire specifies that when a new action procedure is added to a wire, the procedure is immediately run. Explain why this initialization is necessary. In particular, trace through the half-adder example in the paragraphs above and say how the system’s response would differ if we had defined accept-action-procedure! as
;; (define (accept-action-procedure! proc)
;; 	(set! action-procedures
;; 	(cons proc action-procedures)))

; let's look at the or-gate defined in ex. 3.28

;; (define (or-gate a1 a2 output)
;; 	(define (or-action-procedure)
;; 		(let ((new-value
;; 						(logical-or (get-signal a1) (get-signal a2))))
;; 			(after-delay
;; 				or-gate-delay
;; 				(lambda () (set-signal! output new-value)))))
;; 	(add-action! a1 or-action-procedure)
;; 	(add-action! a2 or-action-procedure)
;; 	'ok)


;; ; each gate and inverter uses the add-action! procedure, which is just accept-action-procedure! for each particular wire

;; (define (accept-action-procedure! proc)
;; 	(set! action-procedures
;; 	(cons proc action-procedures)))

;; ;; or

;; (define (accept-action-procedure! proc)
;; 	(set! action-procedures
;; 		(cons proc action-procedures))
;; 	(proc))

;; ;; but! the procedure that it calls is the equivalent of

;; (define (or-action-procedure)
;; 	(let ((new-value
;; 					(logical-or (get-signal a1) (get-signal a2))))
;; 		(after-delay
;; 			or-gate-delay
;; 			(lambda () (set-signal! output new-value)))))

;; ;; which adds the delay and the action to the agenda
;; ; when we run propagate, it first checks the agenda and runs the actions in order

;; (define (propagate)
;; 	(if (empty-agenda? the-agenda)
;; 			'done
;; 			(let ((first-item (first-agenda-item the-agenda)))
;; 				(first-item)
;; 				(remove-first-agenda-item! the-agenda)
;; 				(propagate))))

;; ; if we don't call the procedure before adding it to the wire's procedure list, nothing will be added to the agenda. Only to the wire to-do list. Propagate will find an empty agenda and the simulation won't run

;; (newline)
;; (display "Test 1: accept-action-procedure! defined as normal")
;; (define the-agenda (make-agenda))
;; (define inverter-delay 2)
;; (define and-gate-delay 3)
;; (define or-gate-delay 5)

;; (define input-1 (make-wire))
;; (define input-2 (make-wire))
;; (define sum (make-wire))
;; (define carry (make-wire))

;; (probe 'sum sum)
;; (probe 'carry carry)

;; (half-adder input-1 input-2 sum carry)
;; (set-signal! input-1 1)
;; (newline)
;; (display "Before first propagate agenda: ")
;; (display the-agenda)
;; (propagate)

;; (set-signal! input-2 1)
;; (newline)
;; (display "Before first propagate agenda: ")
;; (display the-agenda)	
;; (propagate)

;; (newline)
;; (newline)
;; (display "Test 2: accept-action-procedure! not calling the procedure")

;; (define test2-the-agenda (make-agenda))

;; (define (test2-make-wire)
;; 	(let ((signal-value 0) (action-procedures '()))
;; 		(define (set-my-signal! new-value)
;; 			(if (not (= signal-value new-value))
;; 					(begin (set! signal-value new-value)
;; 								 (call-each action-procedures))
;; 					'done))
;; 		(define (accept-action-procedure! proc)
;; 			(set! action-procedures (cons proc action-procedures)))
;; 		(define (dispatch m)
;; 			(cond ((eq? m 'get-signal) signal-value)
;; 						((eq? m 'set-signal!) set-my-signal!)
;; 						((eq? m 'add-action!) accept-action-procedure!)
;; 						(else (error "Unknown operation -- WIRE" m))))
;; 		dispatch))

;; (define (test2-half-adder a b s c)
;; 	(let ((d (test2-make-wire)) (e (test2-make-wire)))
;; 		(or-gate a b d)
;; 		(and-gate a b c)
;; 		(inverter c e)
;; 		(and-gate d e s)
;; 		'ok))

;; (define (test2-propagate)
;; 	(if (empty-agenda? test2-the-agenda)
;; 			'done
;; 			(let ((first-item (first-agenda-item test2-the-agenda)))
;; 				(first-item)
;; 				(remove-first-agenda-item! test2-the-agenda)
;; 				(test2-propagate))))


;; (define test2-input-1 (test2-make-wire))
;; (define test2-input-2 (test2-make-wire))
;; (define test2-sum (test2-make-wire))
;; (define test2-carry (test2-make-wire))

;; (probe 'test2-sum test2-sum)
;; (probe 'test2-carry test2-carry)

;; (test2-half-adder test2-input-1 test2-input-2 test2-sum test2-carry)
;; (set-signal! test2-input-1 1)

;; (newline)
;; (display "Before first propagate agenda: ")
;; (display test2-the-agenda)

;; (test2-propagate)

;; (set-signal! test2-input-2 1)
;; (newline)
;; (display "Before second propagate agenda: ")
;; (display test2-the-agenda)
;; (test2-propagate)

;; another point, by chat gpt: Initial Propagation
;; When a new gate or inverter is connected, its output depends on the current signals of its input wires.
;; Running the action procedure immediately ensures that the output wire of the gate or inverter reflects the current state of the input wires right away.


;; Exercise 3.32: the procedures to be run during each time segment of the agenda are kept in a queue. Thus, the procedures for each segment are called in the order in which they were added to the agenda (first in, first out). Explain why this order must be used. In particular, trace the behavior of an and-gate whose inputs change from 0, 1 to 1, 0 in the same segment and say how the behavior would differ if we stored a segment’s procedures in an ordinary list, adding and removing procedures only at the front (last in, first out).

;; ;; reviewing the and-gate

;; (define (and-gate a1 a2 output)
;; 	(define (and-action-procedure)
;; 		(let ((new-value
;; 					 (logical-and (get-signal a1) (get-signal a2))))
;; 			(after-delay and-gate-delay
;; 									 (lambda ()
;; 										 (set-signal! output new-value)))))
;; 	(add-action! a1 and-action-procedure)
;; 	(add-action! a2 and-action-procedure)
;; 	'ok)

;; and-gate adds 2 actions to the agenda, one for each wire	
;; if we have wire a set to 1 and wire b set to 0, the first action in the agenda is:
;; - check signal of a (1), 
;; - check signal of b (0)
;; - set the output according to logical-and (0)
;; the second action in the agenda is the same thing, so in fact output is set twice

;; now, let's look at the case where we set the wires, connect to the and-gate, and then change the wires signals
;; one important thing to note is that when set-signal! is called for a wire, and the signal is the same as before, make-wire will change the signal and call all the actions in the action list:
;; (define (set-my-signal! new-value)
;; 	(if (not (= signal-value new-value))
;; 			(begin (set! signal-value new-value)
;; 						 (call-each action-procedures)) ; this call-each is important
;; 'done))  

;; so, with the agenda setup as a queue as normal, what will happen is:
;; - wire a is set to 0. It is different to the null value as before, but there is nothing in action-procedures, so nothing happens
;; - wire b is set to 1. Same as before
;; - and-gate is called. It will add and-logical-procedure twice to the agenda, and each wire will have its own and-action-procedure to its action-procedures list
;; - wire a is set to 1. It is different as the previous value, so it will change its value and run the and-action-procedure on its list
;; - since wire b is 1, and-action-procedure will set the output to 1 this time
;; - now b is set to 0, and it now runs the and-action-procedure on its list
;; - and since wire a is 1, it will set the output to 0 this time				

(define (test1-and-gate a1 a2 output)
	(define (and-action-procedure)
		(let ((new-value
					 (logical-and (get-signal a1) (get-signal a2))))
			(newline) (display "test1-wire a is: ") (display (get-signal a1))
			(newline) (display "test1-wire b is: ") (display (get-signal a2))
			(newline) (display "new value for test1-output is: ") (display new-value)
			(after-delay and-gate-delay
									 (lambda ()
										 (set-signal! output new-value)))))
	(add-action! a1 and-action-procedure)
	(add-action! a2 and-action-procedure)
	'ok)

(define and-gate-delay 1)
(define test1-wirea (make-wire))
(define test1-wireb (make-wire))
(define test1-output (make-wire))
(set-signal! test1-wirea 0)
(set-signal! test1-wireb 1)
(test1-and-gate test1-wirea test1-wireb test1-output)
(newline)
(display the-agenda)
(set-signal! test1-wirea 1)	
(set-signal! test1-wireb 0)
(newline)
(display the-agenda)
(propagate)
(newline)
(display the-agenda)
(newline)
(display (get-signal test1-output))

;; wire a is: 0
;; wire b is: 1
;; new value for output is: 0
;; wire a is: 0
;; wire b is: 1
;; new value for output is: 0
;; (0 (1 (#[compound-procedure 12] #[compound-procedure 13]) #[compound-procedure 13]))
;; wire a is: 1
;; wire b is: 1
;; new value for output is: 1
;; wire a is: 1
;; wire b is: 0
;; new value for output is: 0
;; (0 (1 (#[compound-procedure 12] #[compound-procedure 13] #[compound-procedure 14] #[compound-procedure 15]) #[compound-procedure 15]))
;; (1)
;; 0
;; ;... done

;; now, supposing we implemented the actions in the agenda as a regular list (first in last out - filo), what should happen is:
;; - wire a is set to 0. It is different to the null value as before, but there is nothing in action-procedures, so nothing happens
;; - wire b is set to 1. Same as before
;; - and-gate is called. It will add and-logical-procedure twice to the agenda, and each wire will have its own and-action-procedure to its action-procedures list (#1 and #2 in the agenda)
;; - wire a is set to 1. It is different as the previous value, so it will change its value and run the and-action-procedure on its list. Since wire b is still 1, it will the output to 1 (#3 in the agenda)
;; - now b is set to 0, and it now runs the and-action-procedure on its list, and since wire a is 1, it will set the output to 0 this time (#4)
;; the agenda will be read at opposite order this time (4, 3, 2, 1) 
;; 	- #4 sets the output to 0
;;  - #3 sets the output to 1
;;  - #2 and 1 don't change the output, so it will keep the output as 1

(define (test2-add-to-agenda! time action agenda)
	(define (belongs-before? segments)
		(newline) (display "segments: ") 	(display segments)
		(or (null? segments)
				(< time (segment-time (car segments)))))
	(define (make-new-time-segment time action)
		(let ((l '()))
				(cons action l)
				(newline) (display "time segme	nt list: ") (display l)
				(make-time-segment time l)
				(newline) (display "time segment: ") (display (make-time-segment time l))))
	(define (add-to-segments! segments)
		(if (= (segment-time (car segments)) time)
				(cons action (car segments))
					(let ((rest (cdr segments)))
						(if (belongs-before? rest)
								(set-cdr!
								 segments
								 (cons (make-new-time-segment time action)
											 (cdr segments)))
								(add-to-segments! rest)))))
	(let ((segments (segments agenda)))
		(newline) (display "belongs-before? ") (display (belongs-before? segments))
		(if (belongs-before? segments)
				(set-segments!
				 agenda
		 (cons (make-new-time-segment time action)
							 segments))
	(add-to-segments! segments))))

(define (test2-make-time-segment time l)
	(cons time l))

(define (test2-after-delay delay action)
(test2-add-to-agenda! (+ delay (current-time test2-the-agenda))
								action
								test2-the-agenda))

(define (test2-and-gate a1 a2 output)
(define (test2-and-action-procedure)
	(let ((new-value
				 (logical-and (get-signal a1) (get-signal a2))))
		(newline) (display "test2-wire a is: ") (display (get-signal a1))
		(newline) (display "test2-wire b is: ") (display (get-signal a2))
		(newline) (display "new value for test2-output is: ") (display new-value)
		(test2-after-delay and-gate-delay
								 (lambda ()
									 (set-signal! output new-value)))))
(add-action! a1 test2-and-action-procedure)
(add-action! a2 test2-and-action-procedure)
'ok)

(define (test2-make-agenda) (list 0))

(define test2-the-agenda (test2-make-agenda))

(define (test2-propagate)
	(if (empty-agenda? test2-the-agenda)
			'done
			(let ((first-item (first-agenda-item test2-the-agenda)))
				(first-item)
				(remove-first-agenda-item! test2-the-agenda)
				(test2-propagate))))

(define and-gate-delay 1)
(define test2-wirea (make-wire))
(define test2-wireb (make-wire))
(define test2-output (make-wire))
(set-signal! test2-wirea 0)
(set-signal! test2-wireb 1)
(test2-and-gate test2-wirea test2-wireb test2-output)
(newline) (display "test2-the-agenda: ") (display test2-the-agenda)
(set-signal! test2-wirea 1)	
(set-signal! test2-wireb 0)
(newline) (display "test2-the-agenda: ") (display test2-the-agenda)
(test2-propagate)
(newline)
(newline) (display "test2-the-agenda: ") (display test2-the-agenda)
(newline)
(display (get-signal test2-output))