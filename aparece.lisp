
(defun aparece (variable lista)
	"Si aparece devuelve sustituto y sustituido, en caso contrario 'nil"
	(if (eq (esVariable variable) 'esVariable)
		(progn
			(setf return_val variable)
			(dolist (item lista)
				; (format t "~D == ~D~%" variable item)
				(case (esVariable item)
					('esVariable (if (eql (first (rest variable)) (first (rest item)))
									(return (setf return_val (first lista)))
									(return (setf return_val variable) ) 
									)
						)
					('lista (prog 
								(let (posibleResultado (aparece variable item)) )
								(if (eq (esVariable posibleResultado) 'atomo)
									(return-from aparece posibleResultado)
									)
								)	 
						)
					)
				)
			; (format t "Resultado: ~D~%" return_val)
			return_val
			)
		)
	)
