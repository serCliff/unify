



(defun esPar (elemento)
	"Busca elementos que sean pares sustituto/sustituido y devuelve true o false en cualquier caso"
	; (format t "Probamos de: ~D -> ~D~%" elemento (first elemento))
	(progn
		(if (eq (esVariable elemento) 'atomo)
			(return-from esPar 'false)
			)
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


(defun composicion (s1 s2)
	"Esta funcion compone de manera recursiva las variables que son sustituto ej (f(x,y)/z) | (A/x B/y) = (f(A,B)/z)"
	(if (eq (esPar s1) 'TRUE) 
		(prog
			(let (primerResultado 'nil))
			(if (eq (esPar S1) 'true) 
				(case (esVariable (first S1) )
					('atomo (setf primerResultado (reverse (cons S1 (reverse primerResultado)))) )
					('lista (setf primerResultado (list (list (aplicar (first S1) s2) (first (rest S1)))) ) )   ;cambiar aplicar por componerLista si hay algún problema
				)
				(case (esVariable S1)
					('lista (setf primerResultado (reverse (cons (composicion S1 s2) (reverse primerResultado)))) )
					('esVariable (setf primerResultado (reverse (cons (aparece S1 s2) (reverse primerResultado)))) )  ;PREGUNTAR SI ESTO HAY QUE HACERLO (COMPONE VARIABLES QUE NO SON PARES)
					(t (setf primerResultado (reverse (cons S1 (reverse primerResultado)))) ) ; ir concatenando los que no se pueden componer variables y atomos
					)
				)
			(return primerResultado)
			)
		(prog
			(let (primerResultado 'nil))
			(dolist (sustituto s1)
				; (format t "Probamos: ~D~%" sustituto)
				(if (eq (esPar sustituto) 'true) 
					(case (esVariable (first sustituto) )
						('atomo (setf primerResultado (reverse (cons sustituto (reverse primerResultado)))) )
						('lista (setf primerResultado (list (list (aplicar (first sustituto) s2) (first (rest sustituto)))) ) )   ;cambiar aplicar por componerLista si hay algún problema
					)
					(case (esVariable sustituto)
						('lista (setf primerResultado (reverse (cons (composicion sustituto s2) (reverse primerResultado)))) )
						('esVariable (setf primerResultado (reverse (cons (aparece sustituto s2) (reverse primerResultado)))) )  ;PREGUNTAR SI ESTO HAY QUE HACERLO (COMPONE VARIABLES QUE NO SON PARES)
						(t (setf primerResultado (reverse (cons sustituto (reverse primerResultado)))) ) ; ir concatenando los que no se pueden componer variables y atomos
						)
					)
				; (format t "Tenemos: ~D~%" primerResultado)
				)
			(return primerResultado)
			)
		)
	)



(defun concatenaResto (primerResultado s2)
	"Esta funcion comprueba que posibles sustituidos quedan por añadir al resultado de componer y los devuelve en una lista"
	; TODO: COMPROBAR SI ES PAR PARA NO RECORRER
	(if (eq (esPar s2) 'TRUE) 
		(prog 
			(let (returnValue 'nil))
			(case (esVariable S2)
				('esVariable (if (eq (aparece S2 primerResultado) S2)
								(setf returnValue (reverse (cons S2 (reverse returnValue))) )
								)
					)
				
				('lista (if (eq (esPar S2) 'true)
							(if (eq (aparece (first (rest S2)) primerResultado) (first (rest S2)) )
								(setf returnValue (reverse (cons S2 (reverse returnValue))) )
								)
							(case (esVariable S2)
								('lista (setf returnValue (reverse (cons (concatenaResto primerResultado S2) (reverse returnValue)))) )
								(t (setf returnValue (reverse (cons S2 (reverse returnValue)))) ) 
								)

							)
						)
					)
			(return returnValue)
			)

		(prog 
			(let (returnValue 'nil))
			(dolist (sustituidos s2)  
				; (format t "Probamos: ~D~%" sustituidos)
				(case (esVariable sustituidos)
					('esVariable (if (eq (aparece sustituidos primerResultado) sustituidos)
									(setf returnValue (reverse (cons sustituidos (reverse returnValue))) )
									)
						)
					
					('lista (if (eq (esPar sustituidos) 'true)
								(if (eq (aparece (first (rest sustituidos)) primerResultado) (first (rest sustituidos)) )
									(setf returnValue (reverse (cons sustituidos (reverse returnValue))) )
									)
								(case (esVariable sustituidos)
									('lista (setf returnValue (reverse (cons (concatenaResto primerResultado sustituidos) (reverse returnValue)))) )
									(t (setf returnValue (reverse (cons sustituidos (reverse returnValue)))) ) 
									)

								)
							)
						)
					)
			(return returnValue)
			)
		)
	)



(defun componer (s1 s2)
	"Funcion componer"
	(format t "~%COMPONER ( ~D , ~D ) ==> " s1 s2)
	(prog
		(let (primerResultado 'nil) (returnValue 'nil) )
		(setf primerResultado (composicion s1 s2) )
		
		; (format t "~%primerResultado: ~D~%" primerResultado)

		(setf finales (concatenaResto primerResultado s2))
		
		; (format t "~%FINALES: ~D~%" finales)

		(if (eq (first finales) 'nil)
			(setf returnValue primerResultado)
			
			(if (eq (first primerResultado) 'nil)
				(setf returnValue finales)
				(setf returnValue (append primerResultado finales))
				)
			)

		(format t "~D~%" returnValue)
		(return returnValue)
		)
	)










