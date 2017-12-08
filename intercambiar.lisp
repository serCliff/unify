


(defun intercambiar (items)
	"Devuelve una lista con los dos elementos ordenados"
	(prog
		(let (e1 (first items)) (e2 (first (rest items))) )
		(if (eq (esvariable e1) 'esVariable)
			(if (eq (esvariable e2) 'esVariable)
				(return (list e1 e2))
				(return (list e2 e1))
				)
			(return (list e1 e2))
		)
	)
)

