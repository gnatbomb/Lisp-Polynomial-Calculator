; CMPUT 325 Winter 2019 Assignment 2
; CCID nbombard Student name Nicholas Bombardieri
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
;	if (eq op +) then reduce-add(expr1 expr2)
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