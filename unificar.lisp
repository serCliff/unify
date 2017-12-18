

(defun equals(e1 e2)
	(prog
		(let (returnValue 'nil) )
		(case (esVariable e1)
			('atomo (if (eq (esVariable e2) 'atomo)
					(if (eq e1 e2)
						(setf returnValue 'true)
						(setf returnValue 'false)
						)
					(setf returnValue 'false)
					)
				)
			('esVariable (if (eq (esVariable e2) 'esVariable)
							(if (eq (first (rest e1)) (first (rest e2)))
									(setf returnValue 'true)
									(setf returnValue 'false)
									)
								(setf returnValue 'false)
							)
				)
			)
		(return returnValue)
		)
	)



(defun primerPaso(e1 e2)
	(format t "IF INICIAL e1: ~D e2: ~D~%" e1 e2)
		(setf returnIf 'nil )

		; (if (eq e1 e2)
		(if (eq (equals e1 e2) 'true)
			(return-from primerPaso returnIf)
			)

		(if (eq (esVariable e1) 'esVariable)
			(if (eq (equals (aparece e1 (list e2)) e1) 'true) 
				(setf returnIf 'FALLO)
				(setf returnIf (list e2 e1))
				)
			)
		(if (eq (esVariable e2) 'esVariable)
			(setf returnIf (list e1 e2))
			)

		returnIf
	)


(defun unificar (e1 e2)
	(format t "unificar( ~D , ~D )~%" e1 e2)
	

	(prog 
		(let (returnUnificar 'nil) (z1 'nil) (z2 'nil) (listaIntercambiada 'nil))

		(if (not (eq (esVariable e1) 'lista))
			(progn 
				(setf listaIntercambiada (intercambiar (list e1 e2)))
				(setf temp e1)
				(setf e1 (first listaIntercambiada))
				(setf e2 (first (rest listaIntercambiada)))
				(setf returnUnificar (primerPaso e1 e2))
				)

			(if (not (eq (esVariable e2) 'lista))
				(progn 
					(setf listaIntercambiada (intercambiar (list e1 e2)))
					(setf temp e1)
					(setf e1 (first listaIntercambiada))
					(setf e2 (first (rest listaIntercambiada)))
					(setf returnUnificar (primerPaso e1 e2))
				)
				(progn
					(format t "CUERPO DE UNIFICAR: e1: ~D e2: ~D~%" e1 e2)

					(setf f1 (first e1))
					(setf t1  (rest e1))
					(format t "F1: ~D T1: ~D~%" F1 T1)
					(setf f2 (first e2))
					(setf t2 (rest e2))
					(format t "F2: ~D T2: ~D~%" F2 T2)
					
					(format t "~%[[ Z1 ]]: ")
					(setf z1 (unificar f1 f2))
					(format t "---> ~D = [[ Z1 ]]~%" z1)

					(if (eq z1 'FALLO)
						(return-from unificar 'nil)
						)
					
					(setf g1 (aplicar t1 z1))
					(format t "~%g1: ~D = aplicar(~D ~D)~%" g1 z1 t1)
					(setf g2 (aplicar t2 z1))
					(format t "g2: ~D = aplicar(~D ~D)~%" g2 z1 t2)

					
					(format t "~%[[ Z2 ]]: ")
					(setf z2 (unificar g1 g2))
					(format t "---> ~D = [[ Z2 ]]~%" z2)
					
					(if (eq z2 'FALLO)
						(return-from unificar 'nil)
						)
					
					(setf returnUnificar (componer z1 z2))
					
					)
			)
		)
		(format t "~%unificar( ~D , ~D ) " e1 e2)
		(return returnUnificar)
	)
)










