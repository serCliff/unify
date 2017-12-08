



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


(defun concatenaResto (primerResultado s2)
	"Esta funcion comprueba que posibles sustituidos quedan por añadir al resultado de componer y los devuelve en una lista"
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



(defun componer (s1 s2)
	"Funcion componer"
	(prog
		(let (primerResultado 'nil) (returnValue 'nil) )
		(setf primerResultado (composicion s1 s2) )
		
		; (format t "~%primerResultado: ~D~%" primerResultado)

		(setf finales (concatenaResto primerResultado s2))

		(if (eq (first finales) 'nil)
			(setf returnValue primerResultado)
			(setf returnValue (append primerResultado finales))
			)

		; (format t "~%COMPONER:~%")
		(return returnValue)
		)
	)













; (defun componerLista (p1 s1)
; 	(prog
; 		(let (returnValue 'nil))
; 		(dolist (posibleSustituido p1)
; 			(case (esVariable posibleSustituido)
; 				('atomo (setf returnValue (reverse (cons posibleSustituido (reverse returnValue)))) )
; 				('esVariable (setf returnValue (prog
; 													(let (value (aparece posibleSustituido s1)))
; 													; (format t "Comparando posibleSustituido: ~D con s1: ~D y tenemos: ~D~%" posibleSustituido s1 value)
; 													(if (not (eq value 'nil))
; 														(return (reverse (cons value (reverse returnValue))))
; 														(return returnValue)
; 														)
; 													)
; 								)
; 					)
; 				('lista (setf returnValue (append returnValue (reverse (list (componerLista posibleSustituido s1))))) ) 
; 				)
; 			; (format t "-> Tenemos: ~D~%" returnValue)

; 			)

; 		(return returnValue)
; 		)
; 	)

; ; (componer '(( (f A (? x)) (? z) ) (A (? y)) (? x) ) '( (B (? h)) ((G (? r))) (? t) ) )
; ; = (((F A (? X)) (? Z)) (A (? Y)) (? X) (B (? H)) (? T))



; ; (componer '( ((g (? x) (? y)) (? z)) ) '( (A (? x)) (B (? y)) (C (? w)) (D (? z)) ) )




; (defun componer (s1 s2)
; 	; FALTA COMPROBAR SI s1 y s2 NO SON DIRECTAMENTE PARES O VARIABLES

; 	(prog
; 		(let (primerResultado 'nil) (returnValue 'nil) )
; 		(setf primerResultado (prog
; 				(let (primerResultado 'nil))
; 				(dolist (sustituto s1)
; 					(if (eq (esPar sustituto) 'true) 
; 						(case (esVariable (first sustituto) )
; 							('atomo (setf primerResultado (reverse (cons sustituto (reverse primerResultado)))) )
; 							('lista (setf primerResultado (list (list (aplicar (first sustituto) s2) (first (rest sustituto)))) ) )   ;cambiar aplicar por componerLista si hay algún problema
; 						)
; 						(setf primerResultado (reverse (cons sustituto (reverse primerResultado))) ) ; ir concatenando los que no componen
; 						)
; 					)
; 				(return primerResultado)
; 				)
; 			)
		
; 		; (format t "~%primerResultado: ~D~%~%" primerResultado)

; 		(setf returnValue 
; 			(prog 
; 				(let (returnValue primerResultado))
; 				(dolist (sustituidos s2)  
; 					(case (esVariable sustituidos)
; 						('esVariable (if (eq (aparece sustituidos primerResultado) sustituidos)
; 										(setf returnValue (reverse (cons sustituidos (reverse returnValue))) )
; 										)
; 							)
; 						('lista (if (eq (esPar sustituidos) 'true)
; 									(if (eq (aparece (first (rest sustituidos)) primerResultado) (first (rest sustituidos)) )
; 										(setf returnValue (reverse (cons sustituidos (reverse returnValue))) )
; 										)
; 									)
; 							)
; 						)
; 					)
; 				(return returnValue)
; 				)
; 		)

; 		(format t "~%COMPONER:~%")
; 		(return returnValue)
; 		)
; 	)
