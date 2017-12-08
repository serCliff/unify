

; POSIBILIDADES

; A/x	   	->	(A (? x))
; f(A)/x 	->	((f A) (? x))
; f(y)/x 	->	((f (? y)) (? x))


(defun aplicar (p1 s1)

	(if (eq p1 nil)
		(if (eq s1 'nil)
			'nil
			s1
			)
		(if (eq s1 'nil)
			p1
			(if (eq (esVariable p1) 'lista)
				(prog
					(let (returnValue 'nil))
					(dolist (posibleSustituido p1)
						(case (esVariable posibleSustituido)
							('atomo (setf returnValue (reverse (cons posibleSustituido (reverse returnValue)))) )
							('esVariable (setf returnValue (prog
																(let (value (aparece posibleSustituido s1)))
																; (format t "Comparando posibleSustituido: ~D con s1: ~D y tenemos: ~D~%" posibleSustituido s1 value)
																(if (not (eq value 'nil))
																	(return (reverse (cons value (reverse returnValue))))
																	(return returnValue)
																	)
																)
											)
								)
							('lista (setf returnValue (append returnValue (reverse (list (aplicar posibleSustituido s1))))) ) 
							)
						; (format t "-> Tenemos: ~D~%" returnValue)

						)

					(return returnValue)
					)
				)
			)
		)
	)