;; Representations for Complex Numbers

;; (make-from-real-imag (real-part z) 
;;                      (imag-part z))

;; (make-from-mag-ang (magnitude z) 
;;                    (angle z))

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

;; Ben's representation

(define (real-part z) (car z))
(define (imag-part z) (cdr z))

(define (magnitude z)
  (sqrt (+ (square (real-part z)) 
           (square (imag-part z)))))

(define (angle z)
  (atan (imag-part z) (real-part z)))

(define (make-from-real-imag x y) 
  (cons x y))

(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))

;; Alyssa's representation

(define (real-part z)
  (* (magnitude z) (cos (angle z))))

(define (imag-part z)
  (* (magnitude z) (sin (angle z))))

(define (magnitude z) (car z))
(define (angle z) (cdr z))

(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))

(define (make-from-mag-ang r a) 
  (cons r a))

;; Tagged data

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: 
              TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: 
              CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

;; Ben's new representation

(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 
   'rectangular
   (cons (* r (cos a)) (* r (sin a)))))

;; Alyssa's new representation

(define (real-part-polar z)
  (* (magnitude-polar z) 
     (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) 
     (sin (angle-polar z))))

(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 
   'polar
   (cons (sqrt (+ (square x) (square y)))
         (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

;; Selectors

(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type: 
               REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type: 
               IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type: 
               MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type: 
               ANGLE" z))))

; the arithmetic procedures are still the same

;; Constructors

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;; Data-Directed Programming and Additivity

(define *op-table* (make-equal-hash-table))

(define (put op type proc)
 	(hash-table-set! *op-table* (list op type) proc))

(define (get op type)
 	(hash-table-ref/default *op-table* (list op type) #f))


(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) 
    (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) 
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

(install-rectangular-package)
(install-polar-package)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types: 
             APPLY-GENERIC"
            (list op type-tags))))))

(define (real-part z) 
  (apply-generic 'real-part z))
(define (imag-part z) 
  (apply-generic 'imag-part z))
(define (magnitude z) 
  (apply-generic 'magnitude z))
(define (angle z) 
  (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 
        'rectangular) 
   x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 
        'polar) 
   r a))

;; Exercise 2.73: 2.3.2 described a program that performs symbolic differentiation:

;; (define (deriv exp var)
;;   (cond ((number? exp) 0)
;;         ((variable? exp) 
;;          (if (same-variable? exp var) 1 0))
;;         ((sum? exp)
;;          (make-sum (deriv (addend exp) var)
;;                    (deriv (augend exp) var)))
;;         ((product? exp)
;;          (make-sum
;;            (make-product 
;;             (multiplier exp)
;;             (deriv (multiplicand exp) var))
;;            (make-product 
;;             (deriv (multiplier exp) var)
;;             (multiplicand exp))))
;;         ;; ⟨more rules can be added here⟩
;;         (else (error "unknown expression type:
;;                       DERIV" exp))))

;; We can regard this program as performing a dispatch on the type of the expression to be differentiated. In this situation the “type tag” of the datum is the algebraic operator symbol (such as +) and the operation being performed is deriv. We can transform this program into data-directed style by rewriting the basic derivative procedure as

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) 
           (if (same-variable? exp var) 
               1 
               0))
         (else ((get 'deriv (operator exp)) 
                (operands exp) 
                var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; 1. Explain what was done above. Why can’t we assimilate the predicates number? and variable? into the data-directed dispatch?

;;;; since we are dispatching the operator to the deriv procedure, if there is no operator (i.e. exp is a number or a variable) the procedure will fail.

;; 2. Write the procedures for derivatives of sums and products, and the auxiliary code required to install them in the table used by the program above.

(define (install-deriv-package)
	(define (deriv-sum exp var)
	         (make-sum (deriv (addend exp) var)
	                   (deriv (augend exp) var)))

	(define (deriv-mul exp var)
	         (make-sum
	           (make-product 
	            (multiplier exp)
	            (deriv (multiplicand exp) var))
	           (make-product 
	            (deriv (multiplier exp) var)
	            (multiplicand exp))))
	
	(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        (else (list '+ a1 a2))))

	(define (addend s) (car s))
	(define (augend s) (cadr s))
	
	(define (make-product m1 m2)
	  (cond ((or (=number? m1 0) 
	             (=number? m2 0)) 
	         0)
	        ((=number? m1 1) m2)
	        ((=number? m2 1) m1)
	        ((and (number? m1) (number? m2)) 
	         (* m1 m2))
	        (else (list '* m1 m2))))

	(define (multiplier p) (car p))
	(define (multiplicand p) (cadr p))
	
	(define (=number? exp num)
  	(and (number? exp) (= exp num)))
	
	;; interface to the rest of the system
	(put 'deriv '+ deriv-sum)
	(put 'deriv '* deriv-mul))

(install-deriv-package)

(define test-exp '(* x (* y (+ x 3))))
(define test-var 'x)

;; (display (deriv test-exp test-var))
;; (newline)
	
;; 3. Choose any additional differentiation rule that you like, such as the one for exponents (Exercise 2.56), and install it in this data-directed system.

(define (install-deriv-expt-package)
	(define (deriv-expt exp var)
		(make-product
							(make-product 
									(exponent exp)
									(make-exponentiation (base exp)
																			 (make-sum (exponent exp) -1)))
							(deriv (base exp) var)))

	(define (make-exponentiation b e)
		(cond ((=number? e 0) 1)
					((=number? e 1) b)
					((and (number? b) (number? e)) (** b e))
					(else (list '** b e))))

	(define (base pow) (car pow))
	(define (exponent pow) (cadr pow))

	(put 'deriv '** deriv-expt))

(install-deriv-expt-package)

(define test-expt-exp '(** x y))

;; (display (deriv test-expt-exp test-var))
;; (newline)

;; 4. In this simple algebraic manipulator the type of an expression is the algebraic operator that binds it together. Suppose, however, we indexed the procedures in the opposite way, so that the dispatch line in deriv looked like
;; ((get (operator exp) 'deriv) 
;;  (operands exp) var)
;; What corresponding changes to the derivative system are required?

;;;; we just need to change the put procedure in the packages

(define (install-new-deriv-package)
	(define (deriv-sum exp var)
	         (make-sum (deriv (addend exp) var)
	                   (deriv (augend exp) var)))

	(define (deriv-mul exp var)
	         (make-sum
	           (make-product 
	            (multiplier exp)
	            (deriv (multiplicand exp) var))
	           (make-product 
	            (deriv (multiplier exp) var)
	            (multiplicand exp))))
	
	(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        (else (list '+ a1 a2))))

	(define (addend s) (car s))
	(define (augend s) (cadr s))
	
	(define (make-product m1 m2)
	  (cond ((or (=number? m1 0) 
	             (=number? m2 0)) 
	         0)
	        ((=number? m1 1) m2)
	        ((=number? m2 1) m1)
	        ((and (number? m1) (number? m2)) 
	         (* m1 m2))
	        (else (list '* m1 m2))))

	(define (multiplier p) (car p))
	(define (multiplicand p) (cadr p))
	
	(define (=number? exp num)
  	(and (number? exp) (= exp num)))
	
	;; interface to the rest of the system
	(put '+ 'deriv deriv-sum) ;;;;;;;; HERE
	(put '* 'deriv  deriv-mul))

(install-new-deriv-package)

(define (install-new-deriv-expt-package)
	(define (deriv-expt exp var)
		(make-product
							(make-product 
									(exponent exp)
									(make-exponentiation (base exp)
																			 (make-sum (exponent exp) -1)))
							(deriv (base exp) var)))

	(define (make-exponentiation b e)
		(cond ((=number? e 0) 1)
					((=number? e 1) b)
					((and (number? b) (number? e)) (** b e))
					(else (list '** b e))))

	(define (base pow) (car pow))
	(define (exponent pow) (cadr pow))

	(put '** 'deriv deriv-expt)) ;;;;;;;; HERE

(install-new-deriv-expt-package)

(define (new-deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) 
           (if (same-variable? exp var) 
               1 
               0))
         (else ((get (operator exp) 'deriv) ;; changing the order of get
                (operands exp) 
                var))))

;; (display (new-deriv test-expt-exp test-var))
;; (newline)

;; (display (new-deriv test-exp test-var))
;; (newline)

;; Exercise 2.74: Insatiable Enterprises, Inc., is a highly decentralized conglomerate company consisting of a large number of independent divisions located all over the world. The company’s computer facilities have just been interconnected by means of a clever network-interfacing scheme that makes the entire network appear to any user to be a single computer. Insatiable’s president, in her first attempt to exploit the ability of the network to extract administrative information from division files, is dismayed to discover that, although all the division files have been implemented as data structures in Scheme, the particular data structure used varies from division to division. A meeting of division managers is hastily called to search for a strategy to integrate the files that will satisfy headquarters’ needs while preserving the existing autonomy of the divisions.

;; Show how such a strategy can be implemented with data-directed programming. As an example, suppose that each division’s personnel records consist of a single file, which contains a set of records keyed on employees’ names. The structure of the set varies from division to division. Furthermore, each employee’s record is itself a set (structured differently from division to division) that contains information keyed under identifiers such as address and salary. In particular:

;; 1. Implement for headquarters a get-record procedure that retrieves a specified employee’s record from a specified personnel file. The procedure should be applicable to any division’s file. Explain how the individual divisions’ files should be structured. In particular, what type information must be supplied?

;;;; the strategy is the same that we used for polar and rectangular representations. We need a procedure that adds a tag for each division and a general get-record that uses this tag to use the correct procedure to get the record.

(define *insatiable-table* (make-equal-hash-table))

(define (insatiable-put op type proc)
 	(hash-table-set! *insatiable-table* (list op type) proc))

(define (insatiable-get op type)
 	(hash-table-ref/default *insatiable-table* (list op type) #f))

(define (usa-records)
	(define records
		'(("001" (john doe 1000 ))
			("002" (albert einstein 2000))))
	(define get-id caar)
	(define (get-record id) 
		(define (iter records id)
			(cond ((equal? (get-id records) id) (cadar records))
						((null? records) (error "Record not found: " id))
						(else (iter (cdr records) id))))
		(iter records id))
	(define (get-salary id) (caddr (get-record id)))
	;;; adding these procedures do *insatiable-table*
	(insatiable-put 'get-id 'usa get-id)
	(insatiable-put 'get-record 'usa get-record)
	(insatiable-put 'get-salary 'usa get-salary)
	(insatiable-put 'records 'usa records))

(usa-records)

(define (brazil-records)
	(define records
		'(("001" joao silva 800)
			("002" maria pereira 1200)))
	(define get-id caar)
	(define (get-record id) 
		(define (iter records id)
			(cond ((equal? (get-id records) id) (cdar records))
						((null? records) (error "Record not found: " id))
						(else (iter (cdr records) id))))
		(iter records id))
	(define (get-salary id) (caddr (get-record id)))
	;;; adding these procedures do *insatiable-table*
	(insatiable-put 'get-id 'brazil get-id)
	(insatiable-put 'get-record 'brazil get-record)
	(insatiable-put 'get-salary 'brazil get-salary)
	(insatiable-put 'records 'brazil records))

(brazil-records)

(define (get-record division id)
	((insatiable-get 'get-record division) id))

;; 2. Implement for headquarters a get-salary procedure that returns the salary information from a given employee’s record from any division’s personnel file. How should the record be structured in order to make this operation work?

(define (get-salary division id)
	((insatiable-get 'get-salary division) id))

;; 3. Implement for headquarters a find-employee-record procedure. This should search all the divisions’ files for the record of a given employee and return the record. Assume that this procedure takes as arguments an employee’s name and a list of all the divisions’ files.

(define (find-employee-record name divisions)
	(if (null? divisions)
			(error "Name not found: " name)
				(let* ((current-division (car divisions))
	       			 (current-records (get-records current-division)))
					(define (iter records name)
						(if (null? records)
								'()
								(let* ((current-id ((get-id current-division) records))
											 (current-record (get-record current-division current-id))
											 (current-name (list (car current-record) (cadr current-record))))
										(if (equal? name current-name)	
												current-record
												(iter (cdr records) name)))))
				(let ((search-result (iter current-records name)))
					(if	(null? search-result) 
							(find-employee-record name (cdr divisions)))
								search-result))))

						
(define (get-records division)
	((lambda (x) (insatiable-get 'records division)) x))

(define (get-id division)
	(insatiable-get 'get-id division))

;; 4. When Insatiable takes over a new company, what changes must be made in order to incorporate the new personnel information into the central system?

;;;; each division must provide get-id, get-record, get-salary and records to the insatiable-table

;; Message passing

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op: 
            MAKE-FROM-REAL-IMAG" op))))
  dispatch)


;; Exercise 2.75: Implement the constructor make-from-mag-ang in message-passing style. This procedure should be analogous to the make-from-real-imag procedure given above.

(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) 
					 (* x (cos y)))
          ((eq? op 'imag-part) 
					 (* x (sin y)))
          ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
          (else
           (error "Unknown op: 
            MAKE-FROM-MAG-ANG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))


;; Exercise 2.76: As a large system with generic operations evolves, new types of data objects or new operations may be needed. For each of the three strategies — generic operations with explicit dispatch, data-directed style, and message-passing-style — describe the changes that must be made to a system in order to add new types or new operations. Which organization would be most appropriate for a system in which new types must often be added? Which would be most appropriate for a system in which new operations must often be added?

;; new types: data-directed style. You just need to add a new package for each new type
;; new operations: message-passing. A new operation can be added just by a new dispatch procedure without colliding with other operations of the same name
;; for simple systems the explicit dispatch is better: easier to debug, etc.