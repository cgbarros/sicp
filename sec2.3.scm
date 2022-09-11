(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;; Exercise 2.53: What would the interpreter print in response to evaluating each of the following expressions?

(list 'a 'b 'c)
;; (a b c)

(list (list 'george))
;; ((george))

(cdr '((x1 x2) (y1 y2)))
;; ((y1 y2))

(cadr '((x1 x2) (y1 y2)))
;; (y1 y2)

(pair? (car '(a short list)))
;; #f

(memq 'red '((red shoes) (blue socks)))
;; #f

(memq 'red '(red shoes blue socks))
;; (red shoes blue socks)

;; Exercise 2.54: Two lists are said to be equal? if they contain equal elements arranged in the same order. For example,

(equal? '(this is a list) 
        '(this is a list))

;; is true, but

(equal? '(this is a list) 
        '(this (is a) list))

;; is false. To be more precise, we can define equal? recursively in terms of the basic eq? equality of symbols by saying that a and b are equal? if they are both symbols and the symbols are eq?, or if they are both lists such that (car a) is equal? to (car b) and (cdr a) is equal? to (cdr b). Using this idea, implement equal? as a procedure.102

;; (define (equal? ls1 ls2)
;; 	(cond ((and (null? ls2) (null? ls1)) true)
;; 				((eq? (car ls1) (car ls2)) (equal? (cdr ls1) (cdr ls2)))
;; 				(else false)))

;; Exercise 2.55: Eva Lu Ator types to the interpreter the expression

(car ''abracadabra)

;; To her surprise, the interpreter prints back quote. Explain.

;; The interpreter will substitute ''abracadabra to (quote (quote abracadabra)), i.e. '(quote abradacabra). The car of that list is 'quote

;; 2.3.2 Symbolic Differentiation

;; assume that we already have (e = expression):

;; (variable? e)          ; Is e a variable?
;; (same-variable? v1 v2) ; Are v1 and v2 the same variable?
;; (sum? e)               ; Is e a sum?
;; (addend e)             ; Addend of the sum e.
;; (augend e)             ; Augend of the sum e.
;; (make-sum a1 a2)       ; Construct the sum of a1 and a2.
;; (product? e)           ; Is e a product?
;; (multiplier e)         ; Multiplier of the product e.
;; (multiplicand e)       ; Multiplicand of the product e.
;; (make-product m1 m2)   ; Construct the product of m1 and m2.

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        (else (error "unknown expression 
                      type: DERIV" exp))))

;; now we need to design the proper selectors and constructors (and predicates)
;; we'll use the same prefix notation as Lisp

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (make-product m1 m2) (list '* m1 m2))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

;; making the constructors simplify the expressions

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) 
             (=number? m2 0)) 
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) 
         (* m1 m2))
        (else (list '* m1 m2))))

;; Exercise 2.56: Show how to extend the basic differentiator to handle more kinds of expressions. For instance, implement the differentiation rule

;; d(u^n)|dx = nu^(n-1) * du|dx

;; by adding a new clause to the deriv program and defining appropriate procedures exponentiation?, base, exponent, and make-exponentiation. (You may use the symbol ** to denote exponentiation.) Build in the rules that anything raised to the power 0 is 1 and anything raised to the power 1 is the thing itself.

(define ** expt) ;; setting ** as the exponent procedure using mit-scheme's expt

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
				((exponentiation? exp) 
				 (make-product
						(make-product 
								(exponent exp)
								(make-exponentiation (base exp)
																		 (make-sum (exponent exp) -1)))
						(deriv (base exp) var)))
        (else (error "unknown expression 
                      type: DERIV" exp))))

(define (make-exponentiation b e)
	(cond ((=number? e 0) 1)
				((=number? e 1) b)
				((and (number? b) (number? e)) (** b e))
				(else (list '** b e))))

(define (base pow) (cadr pow))
(define (exponent pow) (caddr pow))
(define (exponentiation? x) 
	(and (pair? x) (eq? (car x) '**)))


;; Exercise 2.57: Extend the differentiation program to handle sums and products of arbitrary numbers of (two or more) terms. Then the last example above could be expressed as

;; (deriv '(* x y (+ x 3)) 'x)

;; Try to do this by changing only the representation for sums and products, without changing the deriv procedure at all. For example, the addend of a sum would be the first term, and the augend would be the sum of the rest of the terms.

;; (define (make-sum a1 a2 . a3)
;; 	(let ((start-list (append (list a1 a2) a3)))
;; 		(define (iter result sum-list num-sum)
;; 			(cond ((null? sum-list) 
;; 						 	(cond ((= num-sum 0) result)
;; 										((null? (cdr result)) num-sum)
;; 										(else (append result (list num-sum)))))
;; 						((number? (car sum-list))
;; 						 	(iter result (cdr sum-list) (+ num-sum (car sum-list))))
;; 						(else (iter (append result (list (car sum-list))) 
;; 												(cdr sum-list) 
;; 												num-sum))))
;; 		(iter '(+) start-list 0)))
  

(define (addend s) (cadr s))

(define (augend s) 
	(if (= (length s) 3)
			(caddr s)
	(append '(+) (cddr s))))


;; (define (make-product m1 m2 . m3)
;; 		(let ((start-list (append (list m1 m2) m3)))
;; 			(define (iter result mul-list num-mul)
;; 				(cond ((null? mul-list)
;; 							 	(cond ((= num-mul 1) result)
;; 											((null? (cdr result)) num-mul)
;; 											(else (append result (list num-mul)))))
;; 							((=number? (car mul-list) 0) 0)
;; 							((number? (car mul-list)) 
;; 							 (iter result (cdr mul-list) (* num-mul (car mul-list))))
;; 							(else (iter (append result (list (car mul-list))) 
;; 													(cdr mul-list)
;; 													num-mul))))
;; 			(iter '(*) start-list 1)))
					

(define (multiplier p) (cadr p))

(define (multiplicand p) 
	(if (= (length p) 3)
			(caddr p)
			(append '(*) (cddr p))))

;; Exercise 2.58: Suppose we want to modify the differentiation program so that it works with ordinary mathematical notation, in which + and * are infix rather than prefix operators. Since the differentiation program is defined in terms of abstract data, we can modify it to work with different representations of expressions solely by changing the predicates, selectors, and constructors that define the representation of the algebraic expressions on which the differentiator is to operate.

;; Show how to do this in order to differentiate algebraic expressions presented in infix form, such as (x + (3 * (x + (y + 2)))). To simplify the task, assume that + and * always take two arguments and that expressions are fully parenthesized.

(define (make-sum a1 a2) (list a1 '+ a2))
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))
(define (addend s) (car s))
(define (augend s) (caddr s))

(define (make-product m1 m2) (list m1 '* m2))
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

;; The problem becomes substantially harder if we allow standard algebraic notation, such as (x + 3 * (x + y + 2)), which drops unnecessary parentheses and assumes that multiplication is done before addition. Can you design appropriate predicates, selectors, and constructors for this notation such that our derivative program still works?

(define (sum? x) ; can't have * and needs to have +
	(define (iter result x)
				(cond ((null? x) result)
							((eq? (car x) '*) #f)
							((eq? (car x) '+) (iter #t (cdr x)))
							(else (iter result (cdr x)))))
	(if (not (pair? x))
			#f
			(iter #f x)))

(define (addend s)
	(define (iter result s)
		(if (eq? (car s) '+)
				(if (= (length result) 1)
						(car result)
						result)
				(iter (append result (list (car s))) (cdr s))))
	(iter (list (car s)) (cdr s)))

(define (augend s)
		(if (eq? (car s) '+)
				(if (= (length (cdr s)) 1)
						(cadr s)
						(cdr s))
				(augend (cdr s))))

(define (product? x) ; needs to have * and that's it
	(define (iter result x)
				(cond ((null? x) result)
							((eq? (car x) '*) #t)
							(else (iter result (cdr x)))))
	(if (not (pair? x))
			#f
			(iter #f x)))

(define (multiplier p) 
		(if (eq? (cadr p) '*)
				(car p)
				(multiplier (cdr p))))

(define (multiplicand p) 
	(define (iter buffer p)
		(cond ((eq? (cadr p) '*)
					 (display "found the multiplication") (newline)
					 	(cond ((and (null? buffer) 
												(= (length (cddr p)) 1))
									 (caddr p))
									((and (null? buffer)
												(> (length (cddr p) 1)))
									 (cddr p))
									((and (not (null? buffer))
												(= (length (cddr p)) 1)
												(= (length buffer) 1))
									 (list (caddr p) '+ (car buffer)))
									((and (not (null? buffer))
												(= (length (cddr p)) 1)
												(> (length buffer) 1))
									 (list (caddr p) '+ buffer))
									((and (not (null? buffer))
												(> (length (cddr p)) 1)
												(= (length buffer) 1))
									 (list (cddr p) '+ (car buffer)))
									((and (not (null? buffer))
												(> (length (cddr p)) 1)
												(< (length buffer) 1))
									 (list (cddr p) '+ buffer))))
					((null? buffer) (iter (list (car p)) (cdr p)))
					((eq? '+ (car p)) (iter buffer (cdr p)))
					(else (iter (append (append buffer '(+)) (list (car p))) (cdr p)))))
	(iter '() p))
									
(define my-expr '(x + 3 * (x + y + 2)))

;; not working yet

;; 2.3.3 Example: Representing Sets

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) 
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) 
                                 set2)))
        (else (intersection-set (cdr set1) 
                                set2))))

;; Exercise 2.59: Implement the union-set operation for the unordered-list representation of sets.

(define (union-set set1 set2)
	(cond ((and (null? set1) (null? set2))
				 '())
				((null? set1)
				 set2)
				((not (element-of-set? (car set1) set2))
				 (cons (car set1)
							 (union-set (cdr set1) set2)))
				(else (union-set (cdr set1) set2))))

;; Exercise 2.60: We specified that a set would be represented as a list with no duplicates. Now suppose we allow duplicates. For instance, the set {1,2,3} could be represented as the list (2 3 2 1 3 2 2). Design procedures element-of-set?, adjoin-set, union-set, and intersection-set that operate on this representation. How does the efficiency of each compare with the corresponding procedure for the non-duplicate representation? Are there applications for which you would use this representation in preference to the non-duplicate one?

; element-of-set needs to have the same implementation
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set))))) 

; adjoin-set no need to test for element-of-set. Efficiency O(1)
(define (adjoin-set x set) 
	(cons x set))

; intersection-set still needs to test for element-of-set, because we have to remove elements that are not part of both sets. Efficiency O(n^2)
(define (intersection-set set1 set2) 
  (cond ((or (null? set1) (null? set2)) 
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) 
                                 set2)))
        (else (intersection-set (cdr set1) 
                                set2))))

; union-set, on the other hand, doesn't have to test for element of set because we don't care about duplicates. Efficiency O(n)

(define (union-set set1 set2)
	(cond ((and (null? set1) (null? set2))
				 '())
				((null? set1) set2)
				((null? set2) set1)
				(else (cons (car set1)
								 	 (union-set (cdr set1) set2)))))


;; I would use this implementation in cases we want to merge a lot of sets but rarely intersect them or check for elements (and we also don't care much about memory), since adjoining and uniting is much more efficient, but takes much more memory.


;; Sets as ordered lists

;; form elemnt-of-set growth is still O(n), but on average we should check n/2 of the set
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

;; on intersection-set we can reduce from O(n^2) to O(n)
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set 
                         (cdr set1)
                         (cdr set2))))
              ((< x1 x2) (intersection-set 
                          (cdr set1) 
                          set2))
              ((< x2 x1) (intersection-set 
                          set1 
                          (cdr set2)))))))

;; Exercise 2.61: Give an implementation of adjoin-set using the ordered representation. By analogy with element-of-set? show how to take advantage of the ordering to produce a procedure that requires on the average about half as many steps as with the unordered representation.

(define (adjoin-set x set)
	(cond ((null? set) (list x))
				((< x (car set)) (cons x set))
				((= x (car set)) set)
				(else (cons (car set) (adjoin-set x (cdr set))))))

;; Exercise 2.62: Give a Θ(n) implementation of union-set for sets represented as ordered lists.

(define (union-set set1 set2)
	  (cond ((null? set1) set2)
					((null? set2) set1)
					(else (let ((x1 (car set1)) (x2 (car set2)))
					        (cond ((= x1 x2)
					               (cons x1 (union-set 
					                          (cdr set1)
					                          (cdr set2))))
					              ((< x1 x2) 
												 (cons x1 (union-set 
					                          (cdr set1)
					                          set2)))
					              ((< x2 x1) 
												 (cons x2 (union-set 
					                          set1 
					                          (cdr set2)))))))))

;; Seets as Binary trees

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? 
          x 
          (left-branch set)))
        ((> x (entry set))
         (element-of-set? 
          x 
          (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree 
          (entry set)
          (adjoin-set x (left-branch set))
          (right-branch set)))
        ((> x (entry set))
         (make-tree
          (entry set)
          (left-branch set)
          (adjoin-set x (right-branch set))))))

;; Exercise 2.63: Each of the following two procedures converts a binary tree to a list.

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append 
       (tree->list-1 
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1 
              (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

;; 1. Do the two procedures produce the same result for every tree? If not, how do the results differ? What lists do the two procedures produce for the trees in Figure 2.16?

;; let's call them trees A, B and C

(define treeA '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define treeB '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define treeC '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

; tree->list-1 runs itself in the left tree + cons entry + itself in the right tree
; so it traverses from left to right
; tree->list-2  seems to do the same

; A: (1 3 5 7 9 11)
; B: (1 3 5 7 9 11)
; C: (1 3 5 7 9 11)

;; 2. Do the two procedures have the same order of growth in the number of steps required to convert a balanced tree with n elements to a list? If not, which one grows more slowly?

; the only difference seems to be the call to append every time, which makes tree->list-1 at least O(n) for every part of the left branch, which halves everytime, so O(n logN) for each side, which is actually O(n^2). The segund one is O(logN) for each side, which is O(n)

;; Exercise 2.64: The following procedure list->tree converts an ordered list to a balanced binary tree. The helper procedure partial-tree takes as arguments an integer n and list of at least n elements and constructs a balanced tree containing the first n elements of the list. The result returned by partial-tree is a pair (formed with cons) whose car is the constructed tree and whose cdr is the list of elements not included in the tree.

(define (list->tree elements)
  (car (partial-tree 
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size 
             (quotient (- n 1) 2)))
        (let ((left-result 
               (partial-tree 
                elts left-size)))
          (let ((left-tree 
                 (car left-result))
                (non-left-elts 
                 (cdr left-result))
                (right-size 
                 (- n (+ left-size 1))))
            (let ((this-entry 
                   (car non-left-elts))
                  (right-result 
                   (partial-tree 
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree 
                     (car right-result))
                    (remaining-elts 
                     (cdr right-result)))
                (cons (make-tree this-entry 
                                 left-tree 
                                 right-tree)
                      remaining-elts))))))))

;; 1. Write a short paragraph explaining as clearly as you can how partial-tree works. Draw the tree produced by list->tree for the list (1 3 5 7 9 11).

;; - partial-tree returns a balanced tree with the first n elements of the list and the elements not included in the list
;; - to create the left size, it first calculates a new n (left-size) being all the elements before the middle
;; - then it strats constructing the balanced left-tree by using this "new n" calling it left-result (partial-tree elts left-size)
;; - the left-tree is defined as the car of left-results (the tree part of the output) and the elements that will go to the right tree are the cdr of the procedure (the "leftovers")
;; - it then calculates a new "n" which is the number of elements to the right of the middle (- n (+ left-size 1)). By the way: the element in "the middle" is actually in the middle if the number of elts is odd, or it's the one before the half of element, if number of elts is even
;; - since the element in the middle was not included anywhere, it will be our node (this-entry (car non-left-elts)) which later will be the node at (make-tree this-entry left-tree right-tree)
;; - we then calculate the right tree using the same procedure as before, but using as the "new" elts what was left from the calculation of the left-tree (cdr non-left-elts)
;; - once again, we define the right-tree as the car (the tree part) of the calculation before and, if there are still any left-overs, we call them remainin-elts
;; - finally, we construct the resulting tree using the make-tree procedure with this-entry as the node and adjoining the left and right trees plus a cons with the remaining elements, if there are any, so it can be used as remaining-elts to recursive calculations.

;;      5 
;;   1     9
;;     3 7   11


;; 2. What is the order of growth in the number of steps required by list->tree to convert a list of n elements?

;; Since each time we're applying calculations to half of the list, the list needs to double so that we have one more split between left-size and right-size. However, every time we make two calls, so the order of growth is O(n)

;; Exercise 2.65: Use the results of Exercise 2.63 and Exercise 2.64 to give Θ(n) implementations of union-set and intersection-set for sets implemented as (balanced) binary trees.

(define (union-set set1 set2)
	(let ((list-set1 (tree->list-2 set1))
				 (list-set2 (tree->list-2 set2)))
	 (define (helper ordered-set1 ordered-set2)
		 (cond ((null? ordered-set1) ordered-set2)
					 ((null? ordered-set2) ordered-set1)
					 (else (let ((x1 (car ordered-set1)) (x2 (car ordered-set2)))
					         (cond ((= x1 x2)
					                (cons x1 (helper 
					                           (cdr ordered-set1)
					                           (cdr ordered-set2))))
					               ((< x1 x2) 
								 				  (cons x1 (helper 
					                           (cdr ordered-set1)
					                           ordered-set2)))
					               ((< x2 x1) 
												  (cons x2 (helper 
					                           ordered-set1 
					                           (cdr ordered-set2)))))))))
	 (list->tree (helper list-set1 list-set2))))
	  

(define (intersection-set set1 set2)
	(let ((list-set1 (tree->list-2 set1))
				 (list-set2 (tree->list-2 set2)))
	 (define (helper ordered-set1 ordered-set2)
		   (if (or (null? ordered-set1) (null? ordered-set2))
      	'()
	      (let ((x1 (car ordered-set1)) (x2 (car ordered-set2)))
	        (cond ((= x1 x2)
	               (cons x1 (helper 
	                         (cdr ordered-set1)
	                         (cdr ordered-set2))))
	              ((< x1 x2) (helper 
	                          (cdr ordered-set1) 
	                          ordered-set2))
	              ((< x2 x1) (helper 
	                          ordered-set1 
	                          (cdr ordered-set2)))))))
	 (list->tree (helper list-set1 list-set2))))

(define myTreeA '(4 (2 (1 () ()) (3 () ())) (6 (5 () ()) (7 () ()))))
(define myTreeB '(6 (2 () (4 () ())) (8 () (10 () ()))))
(define myTreeC '(5 (1 () (3 () ())) (7 () (9 () ()))))

;; Sets and information retrieval

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key 
                 (key (car set-of-records)))
         (car set-of-records))
        (else 
         (lookup given-key 
                 (cdr set-of-records)))))

;; Exercise 2.66: Implement the lookup procedure for the case where the set of records is structured as a binary tree, ordered by the numerical values of the keys.

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (key (car set-of-records))) (car set-of-records))
        ((< given-key (key (car set-of-records)))
         (lookup
          given-key 
          (left-branch set-of-records)))
        ((> given-key (key (car set-of-records)))
         (lookup 
          given-key 
          (right-branch set-of-records)))))

(define make-record cons)
(define key car)

(define test-records 
  (list->tree
   (list (make-record 1 "andy") 
         (make-record 2 "bob") 
         (make-record 3 "carol") 
         (make-record 4 "deepak") 
         (make-record 5 "ethel")
         (make-record 6 "freidel")
         (make-record 7 "guido")
         (make-record 8 "harold"))))

;; (display (lookup 1 test-records))
;; (newline)
;; (display (lookup 5 test-records))
;; (newline)
;; (display (lookup 8 test-records))

;; Example: Huffman Encoding Trees

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; (define a (make-leaf 'a 8))
;; (define b (make-leaf 'b 3))
;; (define c (make-leaf 'c 1))
;; (define d (make-leaf 'd 1))
;; (define e (make-leaf 'e 1))
;; (define f (make-leaf 'f 1))
;; (define g (make-leaf 'g 1))
;; (define h (make-leaf 'h 1))

;; The decoding procedure

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch 
                (car bits) 
                current-branch)))
          (if (leaf? next-branch)
              (cons 
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) 
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: 
               CHOOSE-BRANCH" bit))))

;; Sets of weighted elements

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

;; Exercise 2.67: Define an encoding tree and a sample message:

(define sample-tree
  (make-code-tree 
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree 
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

(define sample-message 
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;; Use the decode procedure to decode the message, and give the result.

;; > (decode sample-message sample-tree)
;Value: (a d a b b c a)

;; Exercise 2.68: The encode procedure takes as arguments a message and a tree and produces the list of bits that gives the encoded message.

(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) 
                      tree)
       (encode (cdr message) tree))))

;; Encode-symbol is a procedure, which you must write, that returns the list of bits that encodes a given symbol according to a given tree. You should design encode-symbol so that it signals an error if the symbol is not in the tree at all. Test your procedure by encoding the result you obtained in Exercise 2.67 with the sample tree and seeing whether it is the same as the original sample message.

(define (encode-symbol symbol tree)
	(if (not (contains-symbol? symbol tree))
			(error "SYMBOL NOT IN TREE:" symbol)
			(cond ((leaf? tree) '())
						((contains-symbol? symbol (left-branch tree))
						 (cons 0 (encode-symbol symbol (left-branch tree))))
						((contains-symbol? symbol (right-branch tree))
						 (cons 1 (encode-symbol symbol (right-branch tree)))))))

(define (contains-symbol? symbol tree)
	(memq symbol (symbols tree)))

;; Exercise 2.69: The following procedure takes as its argument a list of symbol-frequency pairs (where no symbol appears in more than one pair) and generates a Huffman encoding tree according to the Huffman algorithm.

(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))

;; Make-leaf-set is the procedure given above that transforms the list of pairs into an ordered set of leaves. Successive-merge is the procedure you must write, using make-code-tree to successively merge the smallest-weight elements of the set until there is only one element left, which is the desired Huffman tree. (This procedure is slightly tricky, but not really complicated. If you find yourself designing a complex procedure, then you are almost certainly doing something wrong. You can take significant advantage of the fact that we are using an ordered set representation.)

;; ]=> (make-leaf-set '((A 4) (B 2) (C 1) (D 1)))

;; ;Value: ((leaf d 1) (leaf c 1) (leaf b 2) (leaf a 4))

(define big-tree-list '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))
(define big-tree-set (make-leaf-set big-tree-list))

(define (successive-merge set)
	(define (helper buffer set)
		(cond ((= (length set) 1) set)
					((= (length set) 2) (make-code-tree (car set) (cadr set)))
					(else (let ((item1 (car set))
											(item2 (cadr set))
											(item3 (caddr set)))
									(if (all-same? item1 item2 item3)
											(helper (append buffer (list (car set))) (cdr set))
											(helper '() (append buffer 
																					(cons (make-code-tree (3smallest item1 item2 item3)
																							 			 					  (second-3smallest item1 item2 item3))
																							  (cons (3biggest item1 item2 item3)
																											(cdddr set))))))))))
	(helper '() set))

(define (all-same? item1 item2 item3)
	(and (= (weight item1) (weight item2))
			 (= (weight item2) (weight item3))))

(define (3smallest item1 item2 item3)
	(if (<= (weight item1) (weight item2))
			(if (< (weight item1) (weight item3)) item1 item3)
			(if (< (weight item2) (weight item3)) item2 item3)))

(define (second-3smallest item1 item2 item3)
	(let ((smallest-item (3smallest item1 item2 item3)))
		(define (smallest item1 item2)
			(if (<= (weight item1) (weight item2)) item1 item2))
		(cond ((equal? smallest-item item1) (smallest item2 item3))
					((equal? smallest-item item2) (smallest item1 item3))
					((equal? smallest-item item3) (smallest item1 item2)))))
	

(define (3biggest item1 item2 item3)
	(if (>= (weight item1) (weight item2))
			(if (> (weight item1) (weight item3)) item1 item3)
			(if (> (weight item2) (weight item3)) item2 item3)))

;; much simpler solution:

(define (successive-merge pairs)
    (if (null? (cdr pairs))
        (car pairs)
        (let ((first (car pairs))
              (second (cadr pairs)))
             (successive-merge (adjoin-set
                                      (make-code-tree first second)
                                      (cddr pairs))))))

;; Exercise 2.70: The following eight-symbol alphabet with associated relative frequencies was designed to efficiently encode the lyrics of 1950s rock songs. (Note that the “symbols” of an “alphabet” need not be individual letters.)


;; A    2    NA  16
;; BOOM 1    SHA  3
;; GET  2    YIP  9
;; JOB  2    WAH  1

;; Use generate-huffman-tree (Exercise 2.69) to generate a corresponding Huffman tree, and use encode (Exercise 2.68) to encode the following message:

;; Get a job
;; Sha na na na na na na na na

;; Get a job
;; Sha na na na na na na na na

;; Wah yip yip yip yip 
;; yip yip yip yip yip
;; Sha boom

(define 50s-rock (generate-huffman-tree '((NA 16) (YIP 9) (SHA 3) (A 2) (GET 2) (JOB 2) (BOOM 1) (WAH 1))))

(define encoded-rock-song (encode '(Get a job
																	  Sha na na na na na na na na
																		Get a job
																		Sha na na na na na na na na
																		Wah yip yip yip yip 
																		yip yip yip yip yip
																		Sha boom)
																	50s-rock))

;; > (length encoded-rock-song)
;; ;Value: 84

;; How many bits are required for the encoding? What is the smallest number of bits that would be needed to encode this song if we used a fixed-length code for the eight-symbol alphabet?

;; We would need 3 bits to have 8 symbols (2^3). The song has 36 symbols, therefore we would need 3*36 = 108 bits to encode the song (about 24% more)

;; Exercise 2.71: Suppose we have a Huffman tree for an alphabet of n symbols, and that the relative frequencies of the symbols are 1,2,4,…,2^n−1. Sketch the tree for n=5; for n=10. In such a tree (for general n) how many bits are required to encode the most frequent symbol? The least frequent symbol?

;; n = 5: 1, 2, 4, 8, 16

(define n=5 '((A 16) (B 8) (C 4) (D 2) (E 1)))
(define n=5-tree (generate-huffman-tree n=5))

;; n = 10: 1, 2, 4, 8, 16, 32, 64, 128, 256, 512

(define n=10 '((A 512) (B 256) (C 128) (D 64) (E 32) (F 16) (G 8) (H 4) (I 2) (J 1)))
(define n=10-tree (generate-huffman-tree n=10))

;; bits required to the most frequent symbol = 1
;; bits required to the least frequent symbol = n-1

;; Exercise 2.72: Consider the encoding procedure that you designed in Exercise 2.68. What is the order of growth in the number of steps needed to encode a symbol? Be sure to include the number of steps needed to search the symbol list at each node encountered. To answer this question in general is difficult. Consider the special case where the relative frequencies of the n symbols are as described in Exercise 2.71, and give the order of growth (as a function of n) of the number of steps needed to encode the most frequent and least frequent symbols in the alphabet.

(define (encode-symbol symbol tree)
	(if (not (contains-symbol? symbol tree))
			(error "SYMBOL NOT IN TREE:" symbol)
			(cond ((leaf? tree) '())
						((contains-symbol? symbol (left-branch tree))
						 (cons 0 (encode-symbol symbol (left-branch tree))))
						((contains-symbol? symbol (right-branch tree))
						 (cons 1 (encode-symbol symbol (right-branch tree)))))))

(define (contains-symbol? symbol tree)
	(memq symbol (symbols tree)))

;; - it first checks all the symbols in the tree to see if it is there (worst case n steps)
;; - then it does it again with the left branch
;; - if not in the left branch, it does with the right branch (worst case again, n steps)
;; - now it does the same thing either with the left or right branch. In the worst case we are lowering n by 1 each time. So the worst case would be n + n-1 + n-2 + n-3 + ... + 1. Which is n * n(1 + n)/2. So the order of growth is O(n^2)
;; - on average, though, we would search the most frequent symbols more. So It would probably average to O(n) growth.

;; - in the case of the tree on exercise 2.71, the setps needed to encode the most frequent symbol is 1, and the least frequent n + (n - 1) + (n - 2) ... + 1 ~ O(2n)