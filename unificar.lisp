







(defun primerPaso(e1 e2)
	(format t "~%PRIMER PASO e1: ~D e2: ~D~%" e1 e2)
	(prog
		(let (returnIf 'nil) )
		(if (eq e1 e2)
			(setf returnIf 'nil)
			)
		(if (eq (esVariable e1) 'esVariable)
			(if (not (eq (aparece e1 e2) e1))
				(setf returnIf 'FALLO)
				(setf returnIf (list e2 e1))
				)
			)
		(if (eq (esVariable e2) 'esVariable)
			(setf returnIf (list e1 e2))
			)

		(return returnIf)
		)
	)



(defun cuerpo (e1 e2)
	; (format t "~%CUERPO: e1: ~D e2: ~D~%" e1 e2)
	(prog
		(let (returnCuerpo 'nil) (f1 'nil) (f2 'nil) (t1 'nil) (t2 'nil) )
		
		(setf f1 (first e1))
		(setf t1  (rest e1))
		(format t "~%F1: ~D T1: ~D~%" F1 T1)
		(setf f2 (first e2))
		(setf t2 (rest e2))
		(format t "F2: ~D T2: ~D~%" F2 T2)
		
		(format t "~%Z1: ")
		(setf z1 (unificar f1 f2))
		(format t "Z1: ~D = unificar(~D ~D)~%" z1 f1 f2)

		(if (eq z1 'FALLO)
			(return-from cuerpo 'FALLO)
			)
		
		(setf g1 (aplicar t1 z1))
		(format t "g1: ~D = aplicar(~D ~D)~%" g1 z1 t1)
		(setf g2 (aplicar t2 z1))
		(format t "g2: ~D = aplicar(~D ~D)~%" g2 z1 t2)

		
		(format t "~%Z2: ")
		(setf z2 (unificar g1 g2))
		(format t "Z2: ~D = unificar(~D ~D)~%" z2 g1 g2)
		
		(if (eq z2 'FALLO)
			(return 'FALLO)
			(return (componer z1 z2))
			)
		)
		(return returnCuerpo)
	)


(defun unificar (e1 e2)
	
	(format t "unificar(~D ~D)~%" e1 e2)
		; (let (returnUnificar 'nil) (e1e2 (list e1 e2)) )

	(prog
		(let (returnUnificar 'nil))
		(if (not (eq (esVariable e1) 'lista))
			(case (esVariable e2)
				('atomo (setf returnUnificar (primerPaso e2 e1)))
				('esVariable (setf returnUnificar (primerPaso e1 e2)))
				('lista (setf returnUnificar (cuerpo e1 e2)))
				)
			(setf returnUnificar (cuerpo e1 e2))
			)
		
		; (format t "unificar(~D ~D) = ~D ~%" e1 e2 returnUnificar)
		(return returnUnificar)
		)





		
		; (if (eq (esVariable e2) 'atomo)
		; 	(if (eq (esVariable e1) 'esVariable)
		; 		(setf e1e2 (intercambiar e1e2))
		; 	)
		; )

		
	)