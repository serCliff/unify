
(defun esVariable (var)
    	(cond ((atom var) 'atomo)
		  ((eq (first var) '?) 'esVariable)
		  (t 'lista)
    )
)



