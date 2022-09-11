(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square q) (square p))
                   (+ (square q) (* 2 p q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

 (define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

 (define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

 (define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

 (define odds (list 1 3 5 7))

 (define squares (list 1 4 9 16 25))

 (define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;;  Exercise 2.17.  Define a procedure last-pair that returns the list that contains only the last element of a given (nonempty) list:

;; (last-pair (list 23 72 149 34))
;; (34)

(define (last-pair list)
	(if (null? (cdr list))
			list
			(last-pair (cdr list))))

;; Exercise 2.18.  Define a procedure reverse that takes a list as argument and returns a list of the same elements in reverse order:

(define (reverse-rec list)
	(if (null? list)
			nil
			(append (reverse-rec (cdr list)) (cons (car list) nil))))
	

(define (reverse list)
	(define (iter res list)
		(if (null? list)
				res
				(iter (cons (car list) res) (cdr list))))
	(iter nil list))


;; (reverse (list 1 4 9 16 25))
;; (25 16 9 4 1)

;; Exercise 2.19.  Consider the change-counting program of section 1.2.2. It would be nice to be able to easily change the currency used by the program, so that we could compute the number of ways to change a British pound, for example. As the program is written, the knowledge of the currency is distributed partly into the procedure first-denomination and partly into the procedure count-change (which knows that there are five kinds of U.S. coins). It would be nicer to be able to supply a list of coins to be used for making change.

;; We want to rewrite the procedure cc so that its second argument is a list of the values of the coins to use rather than an integer specifying which coins to use. We could then have lists that defined each kind of currency:

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

;; We could then call cc as follows:

;; (cc 100 us-coins)
;; 292

;; To do this will require changing the program cc somewhat. It will still have the same form, but it will access its second argument differently, as follows:

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

;; Define the procedures first-denomination, except-first-denomination, and no-more? in terms of primitive operations on list structures. Does the order of the list coin-values affect the answer produced by cc? Why or why not?

(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)

(define br-coins-rev (list 1 5 10 25 50 100))
(define br-coins (list 100 50 25 10 5 1))
(define br-coins-2 (list 10 25 1 5 100 50))

;; all produce the same result:
;; (cc 100 br-coins-2)
;; (cc 100 br-coins)
;; (cc 100 br-coins-rev)

;; Exercise 2.20.  The procedures +, *, and list take arbitrary numbers of arguments. One way to define such procedures is to use define with dotted-tail notation. In a procedure definition, a parameter list that has a dot before the last parameter name indicates that, when the procedure is called, the initial parameters (if any) will have as values the initial arguments, as usual, but the final parameter's value will be a list of any remaining arguments. For instance, given the definition

;; (define (f x y . z) <body>)

;; the procedure f can be called with two or more arguments. If we evaluate

;; (f 1 2 3 4 5 6)

;; then in the body of f, x will be 1, y will be 2, and z will be the list (3 4 5 6). Given the definition

;; (define (g . w) <body>)

;; the procedure g can be called with zero or more arguments. If we evaluate

;; (g 1 2 3 4 5 6)

;; then in the body of g, w will be the list (1 2 3 4 5 6).11

;; Use this notation to write a procedure same-parity that takes one or more integers and returns a list of all the arguments that have the same even-odd parity as the first argument. For example,

;; (same-parity 1 2 3 4 5 6 7)
;; (1 3 5 7)

;; (same-parity 2 3 4 5 6 7)
;; (2 4 6)

(define (same-parity n . l)
	(let ((n-list (list n))
				(test? (if (odd? n) odd? even?)))
		(define (iter ans l)
			(cond ((null? l) ans)
						((test? (car l)) (iter (append ans (list (car l))) (cdr l)))
						(else (iter ans (cdr l)))))
		(iter n-list l)))

;; with filter

(define (same-parity-filter n . l)
	(let ((test (if (odd? n) odd? even?)))
		(cons n (filter test l))))

;; Exercise 2.21.  The procedure square-list takes a list of numbers as argument and returns a list of the squares of those numbers.

;; (square-list (list 1 2 3 4))
;; (1 4 9 16)

;; Here are two different definitions of square-list. Complete both of them by filling in the missing expressions:

;; (define (square-list items)
;;   (if (null? items)
;;       nil
;;       (cons <??> <??>)))

;; (define (square-list items)
;; (map <??> <??>))

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list-map items)
(map square items))

;; Exercise 2.22.  Louis Reasoner tries to rewrite the first square-list procedure of exercise 2.21 so that it evolves an iterative process:

;; (define (square-list items)
;;   (define (iter things answer)
;;     (if (null? things)
;;         answer
;;         (iter (cdr things) 
;;               (cons (square (car things))
;;                     answer))))
;;   (iter items nil))

;; Unfortunately, defining square-list this way produces the answer list in the reverse order of the one desired. Why?

;;;;; (cons (square (car things)) answer) will add each answer to the end of the list

;; Louis then tries to fix his bug by interchanging the arguments to cons:

;; (define (square-list items)
;;   (define (iter things answer)
;;     (if (null? things)
;;         answer
;;         (iter (cdr things)
;;               (cons answer
;;                     (square (car things))))))
;;   (iter items nil))

;; This doesn't work either. Explain.

;;;;; this will try to cons a list with a number, which will create a pair (list . number), which is not what we want. Lois needs to use append here.

;; Exercise 2.23.  The procedure for-each is similar to map. It takes as arguments a procedure and a list of elements. However, rather than forming a list of the results, for-each just applies the procedure to each of the elements in turn, from left to right. The values returned by applying the procedure to the elements are not used at all -- for-each is used with procedures that perform an action, such as printing. For example,

;; (for-each (lambda (x) (newline) (display x))
;;           (list 57 321 88))
;; 57
;; 321
;; 88

;; The value returned by the call to for-each (not illustrated above) can be something arbitrary, such as true. Give an implementation of for-each.

(define (for-each proc items)
	(if (null? items)
			 nil
			(begin (proc (car items)) 
						       (for-each proc (cdr items)))))

;; 2.2.2  Hierarchical Structures

(define (count-leaves x)
	(cond ((null? x) 0)
				((not (pair? x)) 1)
				(else (+ (count-leaves (car x))
								 (count-leaves (cdr x))))))

;; Exercise 2.24.  Suppose we evaluate the expression (list 1 (list 2 (list 3 4))). Give the result printed by the interpreter, the corresponding box-and-pointer structure, and the interpretation of this as a tree (as in figure 2.6).

; 1 ]=> (list 1 (list 2 (list 3 4)))
;; Value: (1 (2 (3 4)))

;; pair notation: https://docs.google.com/drawings/d/1NW3GWmihbbxo7wFnGJ5gHyVlzfK7CkVoYIcUP8lVHuo/edit

;; tree notation: https://docs.google.com/drawings/d/1GevgO1M1eHtNjnhtQpZbe-IupHMliG3BtkfKVH9VGDc/edit

;; Exercise 2.25.  Give combinations of cars and cdrs that will pick 7 from each of the following lists:

(define l2.25-1 '(1 3 (5 7) 9))
;; (car (cdr (car (cdr (cdr l2.25-1)))))

(define l2.25-2 '((7)))
;; (caar l2.25-2)

(define l2.25-3  '(1 (2 (3 (4 (5 (6 7)))))))
;; (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l2.25-3))))))))))))

;; Exercise 2.26.  Suppose we define x and y to be two lists:

(define x (list 1 2 3))
(define y (list 4 5 6))

;; What result is printed by the interpreter in response to evaluating each of the following expressions:

;; (append x y)

;; (1 2 3 4 5 6)

;; (cons x y)
;; ((1 2 3) 4 5 6)

;; (list x y)
;; ((1 2 3) (4 5 6)) 

;; Exercise 2.27.  Modify your reverse procedure of exercise 2.18 to produce a deep-reverse procedure that takes a list as argument and returns as its value the list with its elements reversed and with all sublists deep-reversed as well. For example,

;; (define x (list (list 1 2) (list 3 4)))

;; x
;; ((1 2) (3 4))

;; (reverse x)
;; ((3 4) (1 2))

;; (deep-reverse x)
;; ((4 3) (2 1))

;; (define (deep-reverse l)
;; 	(cond ((null? l) nil)
;; 				((pair? (car l)) (append (deep-reverse (cdr l)) (deep-reverse (car l))))
;; 				(else (append (deep-reverse (cdr l)) (cons (car l) nil)))))	

(define (deep-reverse l)
	(cond ((null? l) nil)
			  ((pair? (car l)) (append (deep-reverse (cdr l)) 
																 (list (deep-reverse (car l))))) 
				(else (append (deep-reverse (cdr l)) (list (car l))))))

;; using map: http://community.schemewiki.org/?sicp-ex-2.27
(define (deep-reverse-2 l)
	(if (pair? l) 
			(reverse (map deep-reverse-2 l))
			l))

;; iter (sort of)
(define (deep-reverse-iter l)
	(define (iter l result)
		(cond ((null? l) result)
					((pair? (car l)) (iter (cdr l) (cons (deep-reverse-iter (car l)) result)))
					(else (iter (cdr l) (cons (car l) result)))))
	(iter l nil))

(define my-list '((1 2) (3 4)))
(define my-list-2 '((1 2) (3 (4 5))))

;; (define (reverse list)
;; 	(define (iter res list)
;; 		(if (null? list)
;; 				res
;; 				(iter (cons (car list) res) (cdr list))))
;; 	(iter nil list))

;; Exercise 2.28.  Write a procedure fringe that takes as argument a tree (represented as a list) and returns a list whose elements are all the leaves of the tree arranged in left-to-right order. For example,

(define x (list (list 1 2) (list 3 4)))

;; (fringe x)
;; ;; (1 2 3 4)

;; (fringe (list x x))
;; (1 2 3 4 1 2 3 4)


(define (fringe l)
	(cond((null? l) nil) 
			 ((pair? (car l)) (append (fringe (car l)) (fringe (cdr l))))
			 (else (cons (car l) (fringe (cdr l))))))

;; Exercise 2.29.  A binary mobile consists of two branches, a left branch and a right branch. Each branch is a rod of a certain length, from which hangs either a weight or another binary mobile. We can represent a binary mobile using compound data by constructing it from two branches (for example, using list):

(define (make-mobile left right)
  (list left right))

;; A branch is constructed from a length (which must be a number) together with a structure, which may be either a number (representing a simple weight) or another mobile:

(define (make-branch length structure)
  (list length structure))

;; a.  Write the corresponding selectors left-branch and right-branch, which return the branches of a mobile, and branch-length and branch-structure, which return the components of a branch.

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))

;; b.  Using your selectors, define a procedure total-weight that returns the total weight of a mobile.

(define (total-weight mobile)
	(if (number? mobile)
			mobile
			(+ (total-weight (branch-structure (left-branch mobile)))
				 (total-weight (branch-structure (right-branch mobile))))))

(define my-mobile (make-mobile (make-branch 2 5) 
															 (make-branch 2 (make-mobile (make-branch 1 2)
																												   (make-branch 2 3)))))

;; 1 ]=> (total-weight my-mobile)
;;Value: 10

;; c.  A mobile is said to be balanced if the torque applied by its top-left branch is equal to that applied by its top-right branch (that is, if the length of the left rod multiplied by the weight hanging from that rod is equal to the corresponding product for the right side) and if each of the submobiles hanging off its branches is balanced. Design a predicate that tests whether a binary mobile is balanced.

(define (balanced? mobile)
	(let ((left (left-branch mobile))
				(right (right-branch mobile)))
	(and (= (mobile-torque left) (mobile-torque right))
			 (if (not (number? (branch-structure left)))
					 (balanced? (branch-structure left)
					 #t)
			 (if (not (number? (branch-structure right)))
					 (balanced? (branch-structure right))
					 #t)))))

(define (mobile-torque branch)
	(* (branch-length branch) (branch-weight branch)))

(define (branch-weight branch)
	(if (number? (branch-structure branch))
			(branch-structure branch)
			(total-weight (branch-structure branch))))
			
(define my-mobile-2 (make-mobile (make-branch 2 5) 
															   (make-branch 2 (make-mobile (make-branch 3 2)
																 												     (make-branch 2 3)))))

			
;; d.  Suppose we change the representation of mobiles so that the constructors are

(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

;; How much do you need to change your programs to convert to the new representation?

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cdr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cdr branch))

(define my-mobile-3 (make-mobile (make-branch 2 5) 
															   (make-branch 2 (make-mobile (make-branch 1 2)
																 												     (make-branch 2 3)))))

(define my-mobile-4 (make-mobile (make-branch 2 5) 
															   (make-branch 2 (make-mobile (make-branch 3 2)
																 												     (make-branch 2 3)))))

;; 1 ]=> (total-weight my-mobile-3)
;; ;Value: 10

;; 1 ]=> (total-weight my-mobile-4)
;; ;Value: 10

;; 1 ]=> (balanced? my-mobile-3)
;; ;Value: #f

;; 1 ]=> (balanced? my-mobile-4)
;; ;Value: #t

;; Maping over Trees

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) 
         (* tree factor))
        (else
         (cons (scale-tree (car tree) 
                           factor)
               (scale-tree (cdr tree) 
                           factor)))))

;; using map

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))


;; Exercise 2.30: Define a procedure square-tree analogous to the square-list procedure of Exercise 2.21. That is, square-tree should behave as follows:

;; (square-tree
;;  (list 1
;;        (list 2 (list 3 4) 5)
;;        (list 6 7)))
;; (1 (4 (9 16) 25) (36 49))

;; Define square-tree both directly (i.e., without using any higher-order procedures) and also by using map and recursion.

(define (square-tree tree)
	(cond ((null? tree) nil)
				((not (pair? tree)) (square tree))
				(else (cons (square-tree (car tree))
										(square-tree (cdr tree))))))
 
 (define square-list-1 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

 (define (square-tree tree)
	 (map (lambda (sub-tree)
					(if (not (pair? sub-tree))
							(square sub-tree)
							(square-tree sub-tree))) tree))

  (define square-list-2 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

;; Exercise 2.31: Abstract your answer to Exercise 2.30 to produce a procedure tree-map with the property that square-tree could be defined as

;; (define (square-tree tree) 
  ;; (tree-map square tree))

(define (tree-map proc tree)
	(map (lambda (sub-tree)
				 (if (not (pair? sub-tree))
						 (proc sub-tree)
						 (tree-map proc sub-tree))) tree))

(define (square-tree tree)
	(tree-map square tree))

  (define square-list-3 (list 1
       (list 2 (list 3 4) 5)
			 (list 6 7)))

	;; Exercise 2.32: We can represent a set as a list of distinct elements, and we can represent the set of all subsets of the set as a list of lists. For example, if the set is (1 2 3), then the set of all subsets is (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). Complete the following definition of a procedure that generates the set of subsets of a set and give a clear explanation of why it works:

;; (define (subsets s)
;;   (if (null? s)
;;       (list nil)
;;       (let ((rest (subsets (cdr s))))
;;         (append rest (map ⟨??⟩ rest)))))

;; (subsets '(1 2 3))
;; (append (subsets '(2 3)) (map ?? (subsets '(2 3)))))
;; ?? = (lambda (x) (cons (car s) x))

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

;; aqui estamos fazendo todas as variações de '(2 3) e de '(1) unido com as variações de '(2 3)

;; Sequences as Conventional Interfaces

(define (filter predicate sequence)
	(cond ((null? sequence) nil)
				((predicate (car sequence))
				 (cons (car sequence)
							 (filter predicate (cdr sequence))))
				(else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
	(if (null? sequence)
			initial
			(op (car sequence)
					(accumulate op
											initial
											(cdr sequence)))))

(define ++ (lambda (x) (+ x 1)))
(define -- (lambda (x) (- x 1)))

(define (enumerate-interval low high)
	(if (> low high)
			nil
			(cons low (enumerate-interval (++ low) high))))

(define (enumerate-tree tree)
	(cond ((null? tree) nil)
				((not (pair? tree)) (list tree))
				(else (append
							 (enumerate-tree (car tree))
							 (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
	(accumulate +
							0
							(map square
									 (filter odd?
													 (enumerate-tree tree)))))

(define (even-fibs n)
	(accumulate 
		cons
		nil
		(filter even?
						(map fib
								 (enumerate-interval 0 n)))))

;; Exercise 2.33: Fill in the missing expressions to complete the following definitions of some basic list-manipulation operations as accumulations:

(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) 
              nil sequence))

;; (display (map square '(1 2 3 4)))

(define (append seq1 seq2)
   (accumulate cons seq2 seq1))

;; (display (append '(1 2 3) '(4 5 6)))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(define (length-alt sequence)
	(accumulate + 0 (map (lambda (x) 1) sequence)))

;; (display (length '(1 2 3 4 5 4 3 2 1)))

;; Exercise 2.34: Evaluating a polynomial in x at a given value of x can be formulated as an accumulation. We evaluate the polynomial
;; anxn+an−1xn−1+⋯+a1x+a0
;; using a well-known algorithm called Horner’s rule, which structures the computation as
;; (…(anx+an−1)x+⋯+a1)x+a0.
;; In other words, we start with an, multiply by x, add an−1, multiply by x, and so on, until we reach a0.82

;; Fill in the following template to produce a procedure that evaluates a polynomial using Horner’s rule. Assume that the coefficients of the polynomial are arranged in a sequence, from a0 through an.

(define 
  (horner-eval x coefficient-sequence)
  (accumulate 
   (lambda (this-coeff higher-terms)
     (+ (* higher-terms x) this-coeff))
   0
   coefficient-sequence))

;; For example, to compute 1+3x+5x3+x5 at x=2 you would evaluate

;; (horner-eval 2 (list 1 3 0 5 0 1))

;; (display (horner-eval 2 (list 1 3 0 5 0 1)))

;; Exercise 2.35: Redefine count-leaves from 2.2.2 as an accumulation:

;; (define (count-leaves t)
  ;; (accumulate ⟨??⟩ ⟨??⟩ (map ⟨??⟩ ⟨??⟩)))


(define (count-leaves t)
	(accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

(define (my-count-leaves t)
	(accumulate (lambda (x y) (+ 1 y)) 0 (enumerate-tree t)))

;; (define x (cons (list 1 2) (list 3 4)))
;; (display (count-leaves x)) (newline)

;; (define y (list 1
;;        (list 2 (list 3 4) 5)
;;        (list 6 7)))

;; (display (count-leaves y))
;; (op (car l) (accumulate x y cdr))

;; Exercise 2.36: The procedure accumulate-n is similar to accumulate except that it takes as its third argument a sequence of sequences, which are all assumed to have the same number of elements. It applies the designated accumulation procedure to combine all the first elements of the sequences, all the second elements of the sequences, and so on, and returns a sequence of the results. For instance, if s is a sequence containing four sequences, ((1 2 3) (4 5 6) (7 8 9) (10 11 12)), then the value of (accumulate-n + 0 s) should be the sequence (22 26 30). Fill in the missing expressions in the following definition of accumulate-n:

;; (define (accumulate-n op init seqs)
;;   (if (null? (car seqs))
;;       nil
;;       (cons (accumulate op init ⟨??⟩)
;;             (accumulate-n op init ⟨??⟩))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define x '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

;; Exercise 2.37: Suppose we represent vectors v = (vi) as sequences of numbers, and matrices m = (mij) as sequences of vectors (the rows of the matrix). For example, the matrix

 ;; ⎧ 1 2 3 4 ⎫
 ;; ⎪ 4 5 6 6 ⎪ 
 ;; ⎩ 6 7 8 9 ⎭

;; is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)). With this representation, we can use sequence operations to concisely express the basic matrix and vector operations. These operations (which are described in any book on matrix algebra) are the following:

;; (dot-product v w) returns the sum Σi vi wi;
;; (matrix-*-vector m v) returns the vector t, where ti = Σj mij vj;
;; (matrix-*-matrix m n) returns the matrix p, where pij = Σk mik nkj;
;; (transpose m) returns the matrix n, where nij = mji.

;; We can define the dot product as

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;; Fill in the missing expressions in the following procedures for computing the other matrix operations. (The procedure accumulate-n is defined in Exercise 2.36.)

;; (define (matrix-*-vector m v)
;;   (map ⟨??⟩ m))

;; (define (transpose mat)
;;   (accumulate-n ⟨??⟩ ⟨??⟩ mat))

;; (define (matrix-*-matrix m n)
;;   (let ((cols (transpose n)))
;;     (map ⟨??⟩ m)))


(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product v row)) m))

(define my-matrix '((1 2 3 4) (4 5 6 6) (6 7 8 9)))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

;; (define A '((1 0) (0 2)))
;; (define B '((0 1) (1 0)))
;; (display (matrix-*-matrix A B))
;; (newline)
;; (display (matrix-*-matrix B A))

;; (newline)
;; (newline)

;; (define A '((1 0 1) (2 1 1) (0 1 1) (1 1 2)))
;; (define B '((1 2 1) (2 3 1) (4 2 2)))
;; (display (matrix-*-matrix A B))
;; (newline)
;; (display (matrix-*-matrix B A))

;; Exercise 2.38: The accumulate procedure is also known as fold-right, because it combines the first element of the sequence with the result of combining all the elements to the right. There is also a fold-left, which is similar to fold-right, except that it combines elements working in the opposite direction: 

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; What are the values of

;; (fold-right / 1 (list 1 2 3))
;; (/ 1 (/ 2 (/ 3 1)))
;; 1.5

;; (fold-left  / 1 (list 1 2 3))
;; (/ (/ (/ 1 1) 2) 3)
;; 1/6 ; 0.166666666

;; (fold-right list nil (list 1 2 3))
;; (list 1 (list 2 (list 3 nil)))
;; '(1 (2 (3 ())))


;; (fold-left  list nil (list 1 2 3))
;; (list (list (list nil 1) 2) 3)
;; '(((() 1) 2) 3)

;; Give a property that op should satisfy to guarantee that fold-right and fold-left will produce the same values for any sequence.

;; commutativity

;; Exercise 2.39: Complete the following definitions of reverse (Exercise 2.18) in terms of fold-right and fold-left from Exercise 2.38:

(define (reverse-right sequence)
  (fold-right 
   (lambda (x y) (append y (list x))) nil sequence))

;; (display (reverse-right '(1 2 3)))

 (define (reverse-left sequence)
  (fold-left 
   (lambda (x y) (cons y x)) nil sequence))

; (display (reverse-left '(1 2 3)))

;; Nested Mappings

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) 
        (cadr pair) 
        (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter 
        prime-sum?
        (flatmap
         (lambda (i)
           (map (lambda (j) 
                  (list i j))
                (enumerate-interval 
                 1 
                 (- i 1))))
         (enumerate-interval 1 n)))))

;; permutations of S

;; remove x from S and cons x with the permutations of S-x

(define (permutations s)
  (if (null? s)   ; empty set?
      (list nil)  ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) 
                        (cons x p))
                      (permutations 
                       (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

;; Exercise 2.40: Define a procedure unique-pairs that, given an integer n, generates the sequence of pairs (i,j) with 1≤j<i≤n. Use unique-pairs to simplify the definition of prime-sum-pairs given above.

(define (unique-pairs n)
	(flatmap (lambda (i)
						 (map (lambda (j) (list j i))
									(enumerate-interval 1 (- i 1))))
					 (enumerate-interval 1 n)))

(define (my-prime-sum-pairs n)
  (map make-pair-sum
       (filter 
        prime-sum?
        (unique-pairs n))))

;; Exercise 2.41: Write a procedure to find all ordered triples of distinct positive integers i, j, and k less than or equal to a given integer n that sum to a given integer s.

(define (equal-to-sum k s)
	(map make-pair-sum
			 (filter
					(lambda (x) (= (+ (car x) (cadr x)) s))
					(unique-pairs k))))

;; Exercise 2.42: Eight-queens puzzle

;; adjoin-position vai plugar a posição atual à nova coluna, não o contrário

; current pos : '(1)
; new column: 2
; output: '(2 1)

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position 
                    new-row 
                    ; k ;; I think this is not necessary
                    rest-of-queens))
                 (enumerate-interval 
                  1 
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

(define (adjoin-position new-column current-pos)
			(append current-pos (list new-column)))

(define (kth-element k positions)
	(define (iter i ls)
		(if (= k (+ i 1))
				(car ls)
				(iter (+ i 1) (cdr ls))))
	(iter 0 positions))

(define (safe? k positions)
	(let ((kth (kth-element k positions))
				(test-position (car positions))
				(offset (- k 1)))
		(cond ((= k 1) #t)
					((= kth test-position) #f)            ; same row
					((= kth (+ test-position offset)) #f) ; same diagonal up
					((= kth (- test-position offset)) #f) ; same diagonal down
					(else (safe? (- k 1) (cdr positions))))))

;; working out:

;; primeiro enumera o intervalo de 1 até o tamanho do tabuleiro (ex. (1 2 3 4))
;; deplis mapeia juntar a posição, ou seja, colocar 1 peça em cada posiçao de uma nova coluna (ex. ((1 1) (1 2) (1 3) (1 4) (2 1) (2 2) ... ))

;; (define (adjoin-position new-row rest-of-queens)
;; 			(map (lambda (board) (append board (list new-row))) rest-of-queens))

;; (define current-queens '((1) (2) (3) (4)))

;; (define (test-adjoin)
;; 	(define board-size 4)
;; 	(define rest-of-queens current-queens)
;; 	(flatmap (lambda (new-row)
;;                    (adjoin-position 
;;                     new-row
;; 										0
;;                     rest-of-queens))
;;                  (enumerate-interval 
;;                   1 
;;                   board-size)))

;; (define (my-queens board-size)
;; 	(define (queen-cols k current-board)
;; 		(if (= k board-size)
;; 				(filter 
;; 					 (lambda (current-positions) (safe? k current-positions)) 
;; 				 		current-board)
;; 				(queen-cols (+ k 1)
;; 										(filter (lambda (current-positions) (safe? k current-positions))
;; 														(flatmap (lambda (new-row)
;; 																			 (adjoin-position
;; 																				new-row
;; 																				current-board))
;; 																		 (enumerate-interval 1 board-size))))))
;; 	(queen-cols 1 (map list (enumerate-interval 1 board-size))))

;; ;; depois filtar com "safe?"

;; (define (safe? k positions)
;; 	(if (= k 1)
;; 			#t
;; 			(and (not (same-row? k positions))
;; 					 (not (same-diagonal? k positions)))))

;; (define (same-row? k positions)
;; 	(cond ((= k 1) #f)
;; 				((= (kth-element k positions) (car positions)) #t)
;; 			  (else (same-row? (- k 1) (cdr positions)))))

;; (define (same-diagonal? k positions)
;; 	(let ((kth (kth-element k positions)))
;; 		(cond ((= k 1) #f)
;; 					((or (= kth (+ (car positions) (- k 1)))
;; 							 (= kth (- (car positions) (- k 1))))
;; 				  	#t)
;; 					(else (same-diagonal? (- k 1) (cdr positions))))))
	

;; (define (kth-element k positions)
;; 	(define (iter i ls)
;; 		(if (= k (+ i 1))
;; 				(car ls)
;; 				(iter (+ i 1) (cdr ls))))
;; 	(iter 0 positions))

;; ;; simplificando

;; (define (safe? k positions)
;; 	(let ((kth (kth-element k positions))
;; 				(test-position (car positions))
;; 				(offset (- k 1)))
;; 		(cond ((= k 1) #t)
;; 					((= kth test-position) #f)            ; same row
;; 					((= kth (+ test-position offset)) #f) ; same diagonal up
;; 					((= kth (- test-position offset)) #f) ; same diagonal down
;; 					(else (safe? (- k 1) (cdr positions))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Exercise 2.43: Louis Reasoner is having a terrible time doing Exercise 2.42. His queens procedure seems to work, but it runs extremely slowly. (Louis never does manage to wait long enough for it to solve even the 6×6 case.) When Louis asks Eva Lu Ator for help, she points out that he has interchanged the order of the nested mappings in the flatmap, writing it as

;; (flatmap
;;  (lambda (new-row)
;;    (map (lambda (rest-of-queens)
;;           (adjoin-position 
;;            new-row k rest-of-queens))
;;         (queen-cols (- k 1))))
;;  (enumerate-interval 1 board-size))

;; Explain why this interchange makes the program run slowly. Estimate how long it will take Louis’s program to solve the eight-queens puzzle, assuming that the program in Exercise 2.42 solves the puzzle in time T.

;; - primeiro vai esperar por adjoin-position de quee-cols até k = 0
;; - depois vai juntar todos os intervalos de 1 a board-size (com repetições)
;; - por fim vai filtrar tudo.
;; - portanto, no caso de um tabuleiro de 4x4, por exemplo, ele vai gerar 4^4 posições, enquanto o procedimento normal gera 4!
;; - no caso de 8 damas, a diferença vai ser 8^8 / 8!, aproximadamente 416x mais

(define (wrong-queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
          (adjoin-position 
           new-row 
					 ; k  ; not necessary
					 rest-of-queens))
        (queen-cols (- k 1))))
 (enumerate-interval 1 board-size)))))
  (queen-cols board-size))


;; (with-timings
;;            (lambda () (wrong-queens 7))
;;            (lambda (run-time gc-time real-time)
;;              (write run-time)
;;              (write-char #\space)
;;              (write gc-time)
;;              (write-char #\space)
;;              (write real-time)
;;              (newline)))

;; (wrong-queens 7) 58510 620 122686
;; (queens 7) 260 0 620
;; 7^7 / 7! = 163.4
;; 58510 / 260 = 225
;; 122686 / 620 = 197.8

;; (wrong-queens 6) 3340 40 7105
;; (queens 6) 50 0 118
;; 6^6 / 6! = 64.8
;; 3340 / 50 = 66.8
;; 7105 / 118 = 60.2

;; (wrong-queens 5) 240 0 497
;; (queens 5) 20 0 77
;; 5^5 / 5! = 26.04
;; 240 / 20 = 12
;; 497 / 77 = 6.4


