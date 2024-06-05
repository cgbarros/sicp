(load "sec1.3.com")
(load "sec2.1.com")
(load "sec2.2.com")
(load "sec2.3.com")
(load "sec2.4.com")
(load "sec2.5.com")
(load "sec3.1.com")

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

;; Exercise 3.16: Ben Bitdiddle decides to write a procedure to count the number of pairs in any list structure. “It’s easy,” he reasons. “The number of pairs in any structure is the number in the car plus the number in the cdr plus one more to count the current pair.” So Ben writes the following procedure:

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;; Show that this procedure is not correct. In particular, draw box-and-pointer diagrams representing list structures made up of exactly three pairs for which Ben’s procedure would return 3; return 4; return 7; never return at all.

(define third (cons 'c '()))
(define x (cons 'a (cons 'b third)))
(define second (cons third third))
(define y (cons 'b second))
(define first (cons second second))
(define z first)

;; 1 ]=> x
;; ;Value: (a b c)

;; 1 ]=> y
;; ;Value: (b (c) c)

;; 1 ]=> z
;; ;Value: (((c) c) (c) c)

;; 1 ]=> (count-pairs x)
;; ;Value: 3

;; 1 ]=> (count-pairs y)
;; ;Value: 4

;; 1 ]=> (count-pairs z)
;; ;Value: 7

;; and, of course, with a loop (e.g. third pointing to first) it will never return.

;; Exercise 3.17: Devise a correct version of the count-pairs procedure of Exercise 3.16 that returns the number of distinct pairs in any structure. (Hint: Traverse the structure, maintaining an auxiliary data structure that is used to keep track of which pairs have already been counted.

(define (count-pairs-correct l)
	(define (aux l count seen)
		(cond ((null? l) count)
					((memq (car l) seen) 
					 (aux (cdr l) count seen))
					(else (aux (cdr l) 
										 (++ count) 
										 (append (list (car l)) seen)))))
	(aux l 0 '()))


;; Exercise 3.18: Write a procedure that examines a list and determines whether it contains a cycle, that is, whether a program that tried to find the end of the list by taking successive cdrs would go into an infinite loop. Exercise 3.13 constructed such lists.

(define w (make-cycle (list 'a 'b 'c)))

(define (contains-loop? l)
	(define (aux l seen)
		(cond ((null? (cdr l)) #f)
					((memq (cadr l) seen)
					 #t)
					(else (aux (cdr l) (cons (car l) seen)))))
	(aux l '()))

;; ]=> (contains-loop? x)
;; ;Value: #f

;; ]=> (contains-loop? w)
;; ;Value: #t

;; Exercise 3.19: Redo Exercise 3.18 using an algorithm that takes only a constant amount of space. (This requires a very clever idea.)

(define *op-table* (make-equal-hash-table))

(define (put-pair pair)
 	(hash-table-set! *op-table* pair 'pair))

(define (get-pair pair)
 	(hash-table-ref/default *op-table* pair #f))

(define (contains-loop-constant? l)
	(cond ((null? (cdr l)) #f)
				((get-pair (cadr l))
				 (display (cadr l)) (newline)
				 #t)
				(else 
				 	    (put-pair (car l))
							(contains-loop-constant? (cdr l)))))

;; this algorithm per se is in constant amount of space, but the table grows. The correct solution is the Hare-Tortoise (Floyd's Cycle) algorithm
;; the idea is to have 2 pointers running at different speeds. The fast pointer will either find the list's end or catch the slow one, in which case there is a loop.

(define (contains-loop-constant? l)
	(define (aux slow fast)
		(cond ((null? slow) #f)
					((null? fast) #f)
					((null? (cdr fast)) #f)
					((eq? slow fast) #t)
					(else (aux (cdr slow) (cddr fast)))))
	(aux l (cdr l)))


;; 3.3.2 Representing Queues

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) 
  (set-car! queue item))
(define (set-rear-ptr! queue item) 
  (set-cdr! queue item))

(define (empty-queue? queue) 
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an 
              empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else (set-cdr! (rear-ptr queue) 
                          new-pair)
                (set-rear-ptr! queue new-pair)
                queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with 
                 an empty queue" queue))
        (else (set-front-ptr! 
               queue 
               (cdr (front-ptr queue)))
              queue)))

;; Exercise 3.21: Ben Bitdiddle decides to test the queue implementation described above. He types in the procedures to the Lisp interpreter and proceeds to try them out:

(define q1 (make-queue))

;; (insert-queue! q1 'a)
;; ((a) a)

;; (insert-queue! q1 'b)
;; ((a b) b)

;; (delete-queue! q1)
;; ((b) b)

;; (delete-queue! q1)
;; (() b)

;; “It’s all wrong!” he complains. “The interpreter’s response shows that the last item is inserted into the queue twice. And when I delete both items, the second b is still there, so the queue isn’t empty, even though it’s supposed to be.” Eva Lu Ator suggests that Ben has misunderstood what is happening. “It’s not that the items are going into the queue twice,” she explains. “It’s just that the standard Lisp printer doesn’t know how to make sense of the queue representation. If you want to see the queue printed correctly, you’ll have to define your own print procedure for queues.” Explain what Eva Lu is talking about. In particular, show why Ben’s examples produce the printed results that they do. Define a procedure print-queue that takes a queue as input and prints the sequence of items in the queue.

; as explained in note 150, since empty-queue? looks only at the front pointer, this should be the criteria for an empty list. The interpreter is just showing both pointers while, to see the current state of a queue, we just need to look at the list beginning from the first pointer.

(define (print-queue q)
	(front-ptr q))

;; (show (print-queue (insert-queue! q1 'a)))
;; (show (print-queue (insert-queue! q1 'b)))
;; (show (print-queue (insert-queue! q1 'c)))
;; (show (print-queue (delete-queue! q1)))

;; (a)
;; (a b)
;; (a b c)
;; (b c)

;; Exercise 3.22: Instead of representing a queue as a pair of pointers, we can build a queue as a procedure with local state. The local state will consist of pointers to the beginning and the end of an ordinary list. Thus, the make-queue procedure will have the form

;; (define (make-queue)
;;   (let ((front-ptr … )
;;         (rear-ptr … ))
;;     ⟨definitions of internal procedures⟩
;;     (define (dispatch m) …)
;;     dispatch))

;; Complete the definition of make-queue and provide implementations of the queue operations using this representation.

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
		
    (define (set-front-ptr! item) 
   		(set! front-ptr item))
		(define (set-rear-ptr! item) 
		   (set! rear-ptr item))

		(define (empty-queue?) 
  		(null? front-ptr))
		
		(define (front-queue)
  		(if (empty-queue?)
		      (error "FRONT called with an 
		              empty queue" front-ptr)
		      (car front-ptr)))

		(define (rear-queue)
			(if (empty-queue?)
					(error "REAR called with an empty queue" front-ptr)
					(car rear-ptr)))

(define (insert-queue! item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue?)
           (set-front-ptr! new-pair)
           (set-rear-ptr! new-pair)
           front-ptr)
          (else (set-cdr! rear-ptr new-pair)
                (set-rear-ptr! new-pair)
                front-ptr))))

(define (delete-queue!)
  (cond ((empty-queue?)
         (error "DELETE! called with 
                 an empty queue" print-queue))
        (else (set-front-ptr! (cdr front-ptr))
              front-ptr)))
		
    (define (dispatch m) 
			(cond ((eq? m 'insert) insert-queue!)
						((eq? m 'delete) (delete-queue!))
						((eq? m 'print) front-ptr)
						((eq? m 'front) (front-queue))
						((eq? m 'rear) (rear-queue))
						(else (error "Undefined operation: QUEUE" m))))
    dispatch))

(define q2 (make-queue))

;; Exercise 3.23: A deque (“double-ended queue”) is a sequence in which items can be inserted and deleted at either the front or the rear. Operations on deques are the constructor make-deque, the predicate empty-deque?, selectors front-deque and rear-deque, and mutators front-insert-deque!, rear-insert-deque!, front-delete-deque!, rear-delete-deque!. Show how to represent deques using pairs, and give implementations of the operations. All operations should be accomplished in Θ(1) steps.

;; we need to create a double linked list:
;;
;;                         [ a ][ *-]->[ / ][ *─]─┐
;;                           ▲                    │
;;               ┌───────────┼────────────────────┘
;;               ▼           │
;;             [ b ][ *-]->[ * ][ *─]─┐
;;               ▲                    │
;;   ┌───────────┼────────────────────┘
;;   ▼           │
;; [ c ][ *-]->[ * ][ / ]
;;
;; i.e. (define (new-item data) (cons data (cons '() '())))

(define (make-deque) (cons '() '()))
(define (empty-deque? dq) (and (null? (car dq)) (null? (cdr dq))))

;; useful selectors and mutatores for each item:

(define (dq-next-item item) (cddr item))
(define (dq-previous-item item) (cadr item))
(define (dq-set-previous! item value) (set-car! (cdr item) value))
(define (dq-set-next! item value) (set-cdr! (cdr item) value))

;; front-deque is a pointer to the start of the deque
(define front-deque car)
;; rear-deque is a pointer to the last item
(define rear-deque cdr)

;; front-insert-deque! 
;; - create an item
;; - if the deque is empty, point front and rear to the same item, else
;; - point the new item to front-deque
;; - point the current front-deque item to the new item
;; - set front-deque (the pointer) to the new item

(define (front-insert-deque! item dq)
	(let ((new-item (cons item (cons '() '())))
				(front (front-deque dq)))
		(if (empty-deque? dq)
				(begin (set-car! dq new-item)
							 (set-cdr! dq new-item))
				(begin (dq-set-next! new-item front)
							 (dq-set-previous! front new-item)
							 (set-car! dq new-item)))))

(define dq1 (make-deque))
(front-insert-deque! 'a dq1)
(front-insert-deque! 'b dq1)

;; rear-insert-deque!
;; - create an item
;; - if the deque is empty, point front and rear to the same item
;; - point the new item to rear-deque
;; - point the current rear-deque to the new item
;; - change rear-deque (the pointer) to the new item

(define (front-insert-deque! item dq)
	(let ((new-item (cons item (cons '() '())))
				(rear (rear-deque dq)))
		(if (empty-deque? dq)
				(begin (set-car! dq new-item)
							 (set-cdr! dq new-item))
				(begin (dq-set-previous! new-item rear)
							 (dq-set-next! rear new-item)
							 (set-cdr! dq new-item)))))

(front-insert-deque! 'c dq1)
(front-insert-deque! 'h dq1)

;; front-delete-deque!
;; - if queue is empty - error
;; - if there is nothing after the front - empty deque
;; - else
;;   - set the front pointer to the next item
;; 	 - set the previous pointer of the item to null

(define (front-delete-deque! dq)
	(if (empty-deque? dq) 
			(error "Empty deque --- FRONT-DELETE-DEQUE! " dq)
			(let ((second-item (dq-next-item (front-deque dq))))
				(if (null? second-item)
						(make-deque)
			 			(begin
						 (set-car! dq second-item)
						 (dq-set-previous! second-item '()))))))

;; rear-delete-deque!
;; - if queue is empty - error
;; - if there is nothing before the rear - empty deque
;; - else
;;   - set the rear pointer to the previous item
;; 	 - set the next pointer of the item to null

(define (rear-delete-deque! dq)
	(if (empty-deque? dq) 
			(error "Empty deque --- REAR-DELETE-DEQUE! " dq)
			(let ((next-to-last-item (dq-previous-item (rear-deque dq))))
				(if (null? next-to-last-item)
						(begin (set-car! dq '())
					  			 (set-cdr! dq '()))
			 			(begin
						 (set-cdr! dq next-to-last-item)
						 (dq-set-next! next-to-last-item '()))))))

;; 3.3.3 Representing Tables

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) 
         (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) 
                        (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record 
               (assoc key-2 (cdr subtable))))
          (if record (cdr record) false))
        false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record 
               (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! 
               subtable
               (cons (cons key-2 value)
                     (cdr subtable)))))
        (set-cdr! 
         table
         (cons (list key-1 (cons key-2 value))
               (cdr table)))))
  'ok)

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable 
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record 
                   (assoc key-2 
                          (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable 
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record 
                   (assoc key-2 
                          (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! 
                   subtable
                   (cons (cons key-2 value)
                         (cdr subtable)))))
            (set-cdr! 
             local-table
             (cons (list key-1
                         (cons key-2 value))
                   (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: 
                          TABLE" m))))
    dispatch))

;; Exercise 3.24: In the table implementations above, the keys are tested for equality using equal? (called by assoc). This is not always the appropriate test. For instance, we might have a table with numeric keys in which we don’t need an exact match to the number we’re looking up, but only a number within some tolerance of it. Design a table constructor make-table that takes as an argument a same-key? procedure that will be used to test “equality” of keys. Make-table should return a dispatch procedure that can be used to access appropriate lookup and insert! procedures for a local table.

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
		(define (assoc key records)
		  (cond ((null? records) false)
		        ((same-key? key (caar records)) 
		         (car records))
		        (else (assoc key (cdr records)))))
		
    (define (lookup key-1 key-2)
      (let ((subtable 
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record 
                   (assoc key-2 
                          (cdr subtable))))
              (if record (cdr record) false))
            false)))
		
    (define (insert! key-1 key-2 value)
      (let ((subtable 
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record 
                   (assoc key-2 
                          (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! 
                   subtable
                   (cons (cons key-2 value)
                         (cdr subtable)))))
            (set-cdr! 
             local-table
             (cons (list key-1
                         (cons key-2 value))
                   (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: 
                          TABLE" m))))
    dispatch))

;; Exercise 3.25: Generalizing one-and two-dimensional tables, show how to implement a table in which values are stored under an arbitrary number of keys and different values may be stored under different numbers of keys. The lookup and insert! procedures should take as input a list of keys used to access the table.

(define (make-table same-key?)
	 (let ((local-table (list '*table*)))
		 
		(define (assoc key records)
		  (cond ((null? records) false)
		        ((same-key? key (caar records)) 
		         (car records))
		        (else (assoc key (cdr records)))))
		 
		 (define (lookup-helper key-list table)
			(cond ((null? (car key-list)) #f)
						((null? (cdr key-list))
						 (let ((record (assoc (car key-list) (cdr table))))
							 (if record
									 (cdr record)
									 false)))
						(else (let ((subtable (assoc (car key-list) (cdr table))))
									(lookup-helper (cdr key-list) subtable)))))

		 (define (lookup key-list)
			 (lookup-helper key-list local-table))
		 
		 ; first check if there are still keys in the list
				; if there are, check if the key exists
					; if the key exists navigate through the subtable and keep going
					; if it doesn't, we create a subtable and keep going
				; when we get to the last key, we add the value to the key-value pair. It doesn't matter if the last pair exists or not, if it does we will override it anyway

		(define (add-record! key-list value)
			(if (null? (cdr key-list))
					(cons (car key-list) value)
					(cons (car key-list) (list (add-record! (cdr key-list) value)))))
		
		(define (insert-helper! key-list value table)
			(if (not (null? (cdr key-list)))
					(let ((subtable (assoc (car key-list) (cdr table))))
						(if subtable
								(insert-helper! (cdr key-list) value subtable)
								(set-cdr! table
													(cons (cons (car key-list)
																			(list (add-record! (cdr key-list) value)))
																(cdr table)))))
					(let ((record (assoc (car key-list) (cdr table))))
						(if record
								(set-cdr! record value)
								(set-cdr! table 
													(cons	(cons (car key-list) 
																			value)
																(cdr table)))))))
		 
			(define (insert! key-list value)
				(insert-helper! key-list value local-table))
		 
		(define (dispatch m)
					(cond ((eq? m 'lookup-proc) lookup)
								((eq? m 'insert-proc!) insert!)
								((eq? m 'show) (show local-table))
								(else (error "Unknown operation: 
															TABLE" m))))
		 dispatch))

;;;; tests below

;; (define test-table (make-table eq?))
;; (define (lookup key-list table) 
;; 	((table 'lookup-proc) key-list))
;; (define (insert! key-list value table)
;; 	((table 'insert-proc!) key-list value))


;; (insert! '(test-a) 'a test-table)
;; (insert! '(test-b) 'b  test-table)
;; (show (lookup '(test-a) test-table))
;; (show (lookup '(test-b) test-table))
;; (insert! '(test-a) 'c test-table)
;; (show (lookup '(test-a) test-table))

;; (insert! '(1 a b) 'test1 test-table)
;; (show (lookup '(1 a b) test-table))

;; (insert! '(1 a c) 'test2 test-table)
;; (show (lookup '(1 a c) test-table))

;; (insert! '(b 2) 'test3 test-table)
;; (show (lookup '(b 2) test-table))

;; (test-table 'show)

;; Exercise 3.26: To search a table as implemented above, one needs to scan through the list of records. This is basically the unordered list representation of 2.3.3. For large tables, it may be more efficient to structure the table in a different manner. Describe a table implementation where the (key, value) records are organized using a binary tree, assuming that keys can be ordered in some way (e.g., numerically or alphabetically). (Compare Exercise 2.66 of Chapter 2.)

;; if a table is unidimentional, then the implementation would be the same as ex. 2.66, that is, a key-value set implemented as a tree. The trick is to generalize to n dimensions

;; if not, then each dimension is a tree of keys whose values point to another tree. The last dimension (n) will be a key-value pair. Therefore a table like this:

;; 			1		2		3		4
;;		----------------
;; a	| a1	a2	a3	a4
;; b	| b1	b2	b3	b4
;; c	| c1	c2	c3	c4
;; d	| d1	d2	d3	d4

;; will be represented by a tree like

	;; 		 3
	;; 	  / \
	;; 	 2	 4
	;;  /
	;; 1

;; and each node (key) of the tree has as its value a pointer to a tree of type

	;; 		 c
	;; 	  / \
	;; 	 b	 d
	;;  /
	;; a

;; (i.e.) four trees of the type above, each one with the corresponding values

;; see an implementation of a simple key-pair tree at https://www.inchmeal.io/sicp/ch-3/ex-3.26.html
;; as a project, I could create a table with arbitrary depth - but for that I need to learn how to balance a tree after an insertion.

;; redefining lookup, assoc and insert! for the next exercise:
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) 
         (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) 
                        (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

;; ;; Exercise 3.27: Memoization (also called tabulation) is a technique that enables a procedure to record, in a local table, values that have previously been computed. This technique can make a vast difference in the performance of a program. A memoized procedure maintains a table in which values of previous calls are stored using as keys the arguments that produced the values. When the memoized procedure is asked to compute a value, it first checks the table to see if the value is already there and, if so, just returns that value. Otherwise, it computes the new value in the ordinary way and stores this in the table. As an example of memoization, recall from 1.2.2 the exponential process for computing Fibonacci numbers:

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

;; The memoized version of the same procedure is

;; (define memo-fib
;;   (memoize 
;;    (lambda (n)
;;      (cond ((= n 0) 0)
;;            ((= n 1) 1)
;;            (else 
;;             (+ (memo-fib (- n 1))
;;                (memo-fib (- n 2))))))))

;; where the memoizer is defined as

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result 
             (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize 
   (lambda (n)
     (cond ((= n 0) 0)
           ((= n 1) 1)
           (else 
            (+ (memo-fib (- n 1))
               (memo-fib (- n 2))))))))

;;  If we define memo-fib to be (memoize fib), the procedure won't work, because fib calls itself to compute (fib n), so it doesn't memoize.

;; to understand the syntax better:

(define (my-cons proc) 
	(lambda (x) 
		(cons (proc x) 2)))

;Value: my-cons

(define test 
	(my-cons (lambda (n) (+ n n))))

(show (test 4))
;Value: (8 . 2)