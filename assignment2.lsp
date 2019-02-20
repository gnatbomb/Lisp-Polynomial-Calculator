; CMPUT 325 Winter 2019 Assignment 2
; CCID nbombard Student name Nicholas Bombardieri
(defun test-case (ID Test Result)
    (if (equal Test Result)
        (format nil "Test ~S OK" ID)
        (format nil "FAIL: Test ~S expected ~S got ~S" ID Result Test)
    )
)
;
; Question 1 issorted
;
; issorted(L)
;	if null (cdr L) then T					#if the next element is nil, then list is sorted
;	else if (> (car L) (car (cdr L))) then NIL		#if the current front element is > next element, list is not sorted. return nil.
;	else issorted(cdr L)					#recursive call on list excluding first element
;
(defun issorted (L)
	(if (null (cdr L))
		T
		(if (>= (car L) (cadr L))
			NIL
			(issorted (cdr L))
		)
	)
)
;Question 1 remove-identities
;
; reduce-sum(op expr1 expr2)
;	if (eq expr1 0) then expr2
;	else 
;		if (eq expr2 0) then expr1
;		else cons(op expr1 expr2)
;
(defun reduce-sum (op expr1 expr2)
	(if (eq expr1 0)
		expr2
		(if (eq expr2 0)
			expr1
			(list op expr1 expr2)
		)
	)
)
;
; reduce-mult(expr1 expr2)
;	if (eq expr1 1) then expr2
;	else 
;		if (eq expr2 1) then expr1
;		else list(* expr1 expr2)
;
(defun reduce-mult (expr1 expr2)
	(if (eq expr1 1)
		expr2
		(if (eq expr2 1)
			expr1
			(list '* expr1 expr2)
		)
	)
)
;
; reduce-expression(op expr1 expr2)
;	if (member op '(+ -)) then reduce-add(expr1 expr2)
;	else 
;		if (equal op *) then reduce-mult(expr1 expr2)
;		else (list op expr1 expr2)
;
(defun reduce-expression (op expr1 expr2)
	(if (member op '(+ -))
		(reduce-sum op expr1 expr2)
		(if (eq op '*)
			(reduce-mult expr1 expr2)
			(list op expr1 expr2)
		)
	)
)
;
; remove-identities(E)
;	if (atom E) then E
;	else (
;		reduce-expression(car(E) remove-identities(cdar(E)) remove-identities(cddr(E)))
;	)
;
(defun remove-identities (E)
	(if (atom E)
		E
		(reduce-expression 
			(car E) 
			(remove-identities (cadr E)) 
			(remove-identities (caddr E))
		)
	)
)
;
;
; Question 2 simplify
;
;
; reduce-zeroes(op expr1 expr2)
;	if (and (eq op '-) (eq expr1 expr2)) then 0
;	else 
;		if (and (equal op *) (member 0 '(expr1 expr2)) then 0
;		else (list op expr1 expr2)
;
(defun reduce-zeroes (op expr1 expr2)
	(if (and (eq op '-) (equal expr1 expr2))
		0
		(if (and (equal op '*) (or (eq expr1 0) (eq expr2 0)))
			0
			(list op expr1 expr2)
		)
	)
)
;
; simplify-zeroes (E)
;	if (atom E) then E
;	else (
;		reduce-zeroes(car(E) remove-identities(cdar(E)) remove-identities(cddr(E)))
;	)
(defun simplify-zeroes (E)
	(if (atom E)
		E
		(reduce-zeroes 
			(car E) 
			(simplify-zeroes (cadr E)) 
			(simplify-zeroes (caddr E))
		)
	)
)
;
;
; Question 3 simplify
;
; simplify (E)
;	if (null E) then E 
;	else let*
;		(	
;		(r (remove-identities E))
;		(s (simplify-zeroes r))
;		if (eq E s) then E 
;		else (simplify s)
;		)
;		
(defun simplify (E)
	(if (null E)
		E
		(let*
			(
			(r (remove-identities E))
			(s (simplify-zeroes r))
			)
			(if (equal E s)
				s
				(simplify s)
			)
		)
	)
)
;
;
; Question 3 Bonus
; I believe that the order of calls to remove-identities (henceforth called RI) and simplify-zeroes (henceforth 
;	called SZ) does not matter.
; Note that the domains of RI and SZ are mutually exclusive, meaning that no binary expression which RI affects
; 	will be affected by SZ, and vice versa.
; ASOC there is an expression E which has different results based on the call order to RI and SZ.
; For this to be the case, there must exist a binary expression e which can be modified both by RI and by SZ.
; Contradiction.
; Therefore, order of calling RI and SZ doesn't matter.
;
;
; Question 4 Normalize
;
;
; remove-zero-coefficient (L)
; 	if (null L) then NIL
;	else
;		if (eq (caar L) 0) then remove-zero-coefficient (cdr L)
;		else then cons (car L) (remove-zero-coefficient (cdr L))
;
(defun remove-zero-coefficient (L)
	(if (null L)
		NIL
		(if (eq (caar L) 0)
		(remove-zero-coefficient (cdr L))
		(cons (car L) (remove-zero-coefficient (cdr L)))
		)
	)
)
;
; larger-exponent (L1 L2)
;	(> (cdr L1) (cdr L2))
;
(defun larger-exponent (L1 L2)
	(> (cdr L1) (cdr L2))
)
;
; sort-by-exponent (L)
; 	(sort L 'larger-exponent)
;
(defun sort-by-exponent (L)
	(sort L 'larger-exponent)
)
;
; sum-neighbours (L)
;	if (null (cddr L) then nil
;	else then
;		if (eq (cdar L) (cdadr L)) then (cons (cons (+ (caar L) (caadr L)) (cdar L)) (sum-neighbours (cddr L)))
;		else (cons (car L) (sum-neighbours (cdr L))
;
(defun sum-neighbours (L)
	(if (null (cddr L))
		nil
		(if (eq (cdar L) (cdadr L))
			(cons (cons (+ (caar L) (caadr L)) (cdar L)) (sum-neighbours (cddr L)))
			(cons (car L) (sum-neighbours (cdr L)))
		)
	)
)
;
; combine (L)
;
; normalize (P)
;		sort-by-exponent (remove-zero-coefficient P)
;		
;
(defun normalize (P)
	(sort-by-exponent (remove-zero-coefficient P))
)