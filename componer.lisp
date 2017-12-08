



(defun esPar (elemento)
	; (format t "Probamos de: ~D -> ~D~%" elemento (first elemento))
	(progn
		(case (esVariable (first elemento))
			('esVariable (return-from esPar 'false) )
			(t (case ( esVariable (first (rest elemento)) )
					('esVariable (return-from esPar 'true) )
					(t (return-from esPar 'false))
					)
				)
			)
		)
	)


(defun componerLista (p1 s1)
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
				('lista (setf returnValue (append returnValue (reverse (list (componerLista posibleSustituido s1))))) ) 
				)
			; (format t "-> Tenemos: ~D~%" returnValue)

			)

		(return returnValue)
		)
	)



(defun componer (s1 s2)
	(prog
		(let (primerResultado 'nil) (returnValue 'nil) )
		(dolist (sustituto s1)
			(if (eq (esPar sustituto) 'true) 
				(case (esVariable (first sustituto) )
					('atomo (setf primerResultado (reverse (cons sustituto (reverse primerResultado)))) )
					('lista (setf primerResultado (list (list (componerLista (first sustituto) s2) (first (rest sustituto)))) ) )
				)
				; (format t "Es par: ~D~%" (esPar sustituto) )
				(setf primerResultado sustituto) ; ir concatenando los que no componen
				)
			)
		(return primerResultado)
		)
	)
	

	; (prog 
	; 	(dolist (sustituto s1)
	; 		(format t "Es par: ~D~%" (esPar sustituto))
	; 		; (case (esVariable (first sustituto))
	; 		; 	('atomo (setf returnValue (cons (reverse (sustituto)))) )
	; 		; 	('lista (setf returnValue (cons (reverse (sustituto)))) )
	; 		; 	)
	; 		; )
	; 	)
	; )




