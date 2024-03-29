; CMPUT 325 Winter 2019 Assignment 2
; CCID nbombard Student name Nicholas Bombardieri
;
;
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
;	if (null (cdr L) then nil
;	else then
;		if (eq (cdar L) (cdadr L)) then (cons (cons (+ (caar L) (caadr L)) (cdar L)) (sum-neighbours (cddr L)))
;		else (cons (car L) (sum-neighbours (cdr L))
;
(defun sum-neighbours (L)
	(if (null (cdr L))
		L
		(if (eq (cdar L) (cdadr L))
			(cons (cons (+ (caar L) (caadr L)) (cdar L)) (sum-neighbours (cddr L)))
			(cons (car L) (sum-neighbours (cdr L)))
		)
	)
)
;
;
; normalize (P)
;		sort-by-exponent (remove-zero-coefficient P)
;
(defun normalize (P)
	(remove-zero-coefficient (sum-neighbours (sort-by-exponent P)))
)
;
;
; Question 5 polynomial
;
; Question 5.1.1 poly-add
;
; poly-add (p1 p2)
;	normalize (cons p1 p2)
(defun poly-add (p1 p2)
	(normalize (append p1 p2))
)
;
;
; Question 5.1.2 poly-subtract
;
; poly-neg (P)
;	(cons (- 0 (car P)) (cdr P))
(defun poly-neg (P)
	(cons (- 0 (car P)) (cdr P))
)
;
; poly-subtract (p1 p2)
;	poly-add (p1 (mapcar 'poly-neg p2))
(defun poly-subtract (p1 p2)
	(poly-add p1 (mapcar 'poly-neg p2))
)
;
;
; Question 5.2 poly-multiply
;
; multiply (p1 p2)									
;	(cons (* (car p1) (car p2)) (+ (cdr p1) (cdr p2)))
;
(defun multiply (p1 p2)
	(cons (* (car p1) (car p2)) (+ (cdr p1) (cdr p2)))
)
;
; poly-multiply-helper (p1 p2)
;	if (null p1) then nil
;	else (append (mapcar (lambda (x) (multiply x (car p1)) p2) (poly-multiply (cdr p1) (p2))))
(defun poly-multiply-helper (p1 p2)
	(if (null p1)
		nil
		(if (null p2) 
			nil
			(append (mapcar (lambda (x) 
					(funcall #'multiply x (car p1)) )p2) 
					(poly-multiply (cdr p1) p2))
		)
	)
)
;
; poly-multiply (p1 p2)
;	normalize(poly-multiply-helper p1 p2)
(defun poly-multiply (p1 p2)
	(normalize (poly-multiply-helper p1 p2))
)
;
;
; Question 5.3 polynomial
;
; translate L
;	if (null L) then nil
;	else if (null (atom (car L))) then (list (translate (car L)) (translate (cdr L)))
;		 else if (integerp (car L)) then (list (cons (car L) 0) (translate (cdr L)))
; 			  else if (equal 'x (car L)) then (list (cons 1 1) (translate (cdr L)))
;			  	   else (translate (cdr L))
(defun translate (L)
	(if (null L)
		nil
		(if (null (atom L))
			(cons (translate (car L)) (translate (cdr L)))
			(if (integerp L) 
				(cons (cons L 0) nil)
				(if (equal 'x L)
					(cons (cons 1 1) nil)
					L
				)				
			)
		)
	)
)
;
;
;(defun translate (L)
;	(if (atom L)
;		(cons (trans L) nil)
;		(trans L)
;	)
;)
;
;
; is-pair p1
;	if (atom p1) then false
;	else (and (atom (car p1)) (atom (cdr p1)))
(defun is-pair (p1)
	(if (atom p1)
		nil
		(and (integerp (car p1)) (integerp (cdr p1)))
	)

)
;
; is-poly p1
;	if (null p1) then true
;	else if (atom p1) then false
;		 else then (and (is-pair (car p1)) (is-poly (cdr p1)))
;
(defun is-poly (p1)
	(if (null p1)
		T
		(if (atom p1)
			nil
			(and (is-pair (car p1)) (is-poly (cdr p1)))
		)
	)
)
;
;
(defun makepair (L)
	(if (is-poly L)
		L
		(cons L nil)
	)
)
;
;
; Poly-helper P
; 	if (null P) then P
;	else if (null (atom P)) then (cons (poly-helper (car P)) (poly-helper (cdr P)))
;		 else if (eq '+ p) then (poly-add (poly-helper (cadr P)) (poly-helper (caddr P)))
;			  else if (eq '- p) then (poly-subtract (poly-helper (cadr P)) (poly-helper (caddr P)))
;					else if (eq '* p) then (poly-multiply (poly-helper (cadr P)) (poly-helper (caddr P)))
;						 else P
(defun poly-helper (P)
	(if (null P)
		P
		(if (is-poly p)
			P
			(if (null (atom (car P)))
				(cons (poly-helper (car P)) (poly-helper (cdr P)))
				(if (eq '+ (car p)) 
					(poly-add (makepair (poly-helper (cadr P))) (makepair (poly-helper (caddr P))))
					(if (eq '- (car p))
						(poly-subtract (poly-helper (cadr P)) (poly-helper (caddr P)))
						(if (eq '* (car p))
							(poly-multiply (poly-helper (cadr P)) (poly-helper (caddr P)))
							P
						)				
					)
				)
			)
		)
	)
)
;
; Polynomial P
;	if (null P) then P
;	else (poly-helper (translate P))
(defun polynomial (P)
	(if (null P)
		P
		(poly-helper (translate P))
	)
)
;
;
; Question 6 Print-pexpr
;
;
; concat p1 p2
;	concatenate 'string p1 p2
(defun concat (p1 p2)
	(concatenate 'string p1 p2)
)
;
;
; concatlist L1
;	if (null L1) then L1
;	else (concat (car l1) (concatlist (cdr l1)))
(defun concatlist (l1)
	(if (null L1)
		L1
		(concat (car L1) (concatlist (cdr L1)))
	)
)
;
; get-sign p1
;	if (>= p1 0) then (string-downcase " + ")
;	else then (string-downcase " - ")
;
(defun get-sign (p1)
	(if (>= p1 0)
		(string-downcase " + ")
		(string-downcase " - ")
	)
)
;
; 
; get-abs p1
;	if (< p1 0) then (- 0 p1)
;	else p1
;
(defun get-abs (p1)
	(if (< p1 0) 
		(- 0 p1)
		p1
	)
)
;
;
; get-value p1
;	if (eq (cdr p1) 0) then (write-to-string (get-abs p1))
;	else (concat (car p1) (cdr p1))
;
;
(defun get-value (p1)
	(if (eq (cdr p1) 0)
		(write-to-string (get-abs (car p1)))
		(if (eq 1 (car p1))
			(if (eq (cdr p1) 1)
				(string-downcase "x")
				(concat (string-downcase "x^") (write-to-string (cdr p1)))
			)
			(if (eq (cdr p1) 1)
				(concat (write-to-string (get-abs (car p1))) (string-downcase "x"))
				(concat (write-to-string (get-abs (car p1))) (concat (string-downcase "x^") (write-to-string (cdr p1))))
			)
		)
	)
)
;
; print-it p1
;	(concat (get-sign (car p1)) (get-value p1))
(defun print-it (p1)
	(concat (get-sign (car p1)) (get-value p1))
)
;
; print-first p1
;	if (< (car p1) 0) then (concat (write-to-string "-") (get-value p1))
(defun print-first (p1)
	(if (< (car p1) 0)
		(concat (write-to-string '-) (get-value (cons (get-abs (car p1)) (cdr p1))))
		(get-value p1)
	)
)
;	
;
;
;
; print-pexpr P
;	if (null p) then (write-to-string 0)
;	else (mapcar print-it P)
;
(defun print-pexpr (P)
	(if (null p)
		(write-to-string 0)
		(let* 
			(
				(L (cons (print-first (car p)) (mapcar 'print-it (cdr P))))
			)
		(concatlist L)
		)
	)
)

