;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Logica) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;----------------------------------------------------------------------------------------------------------------------
;;Conjunto de funciones genericas
;;----------------------------------------------------------------------------------------------------------------------
;; Compara dos listas y retorna bool dependiendo de su igualdad. 
(define (comparaListas lista1 lista2 bool)
(cond ((empty? lista1) bool)  
      ((equal? (car lista1) (car lista2)) (comparaListas (cdr lista1) (cdr lista2) #t ))
      (else (comparaListas '() '() #f))
  )
)

;;Cuenta los elementos de una lista.
(define (contadorDeElementos lista)
(cond ((empty? lista) 0)
      (else (contadorElementosAux lista 0))
)
)
(define (contadorElementosAux lista numeroElementos)
(cond((empty? lista) numeroElementos)
     (else (contadorElementosAux (cdr lista) (+ numeroElementos 1)))
     )
 )

;;Quicksort, entra una lista desordenada y retorna la lista ordenada.

(define (Quicksort listaDes)
  (cond ((null? listaDes) listaDes)
        (else (Quicksort_aux (car listaDes) (cdr listaDes)))))


(define (Quicksort_aux pivote lista)
  (cond ((null? lista) (list pivote))
        (else (append (Quicksort (listaMenores pivote lista '())) (list pivote) (Quicksort (listaMayores pivote lista '()))))))

(define (listaMenores pivote listaDesordenada listaOrdenada)
(cond ((empty? listaDesordenada) listaOrdenada)
      ((> pivote (car listaDesordenada)) (listaMenores pivote (cdr listaDesordenada) (append listaOrdenada (list (car listaDesordenada)))))
      (else (listaMenores pivote (cdr listaDesordenada) listaOrdenada))
      )
  )
  
(define (listaMayores pivote listaDesordenada listaOrdenada)
(cond ((empty? listaDesordenada) listaOrdenada)
      ((<= pivote (car listaDesordenada)) (listaMayores pivote (cdr listaDesordenada) (append listaOrdenada (list (car listaDesordenada)))))
      (else (listaMayores pivote (cdr listaDesordenada) listaOrdenada))
      )
  )


;;**********************************************************************************************************************

;;----------------------------------------------------------------------------------------------------------------------
;;FUNCIONES GENERALES PARA LA INTERFAZ
;;----------------------------------------------------------------------------------------------------------------------
;;Funcion principal para obtener la primera generacion
(define (Primera_Generacion Formacion1 Formacion2 Generaciones)
  (generaEquipos (list Formacion1 Formacion2 Generaciones) '() 1))

;; Funcion principal que realiza el proceso del algoritmo genético
;; Equipos = (Equipo1 Equipo2),  Bola = (Bolax Bolay)
(define (Genética Equipos Bola)
  (Genética_aux Equipos Equipos '() Bola))

;; Obtiene el fitness de los equipos y los envia a seleccion
(define (Genética_aux Equipos Equipos_enviar Fitness Bola)
  (cond ((null? Equipos)
         (Seleccion Equipos_enviar Fitness '()))
        (else (Genética_aux (cdr Equipos) Equipos_enviar (append Fitness (list (Fitness_por_equipo (car Equipos) Bola))) Bola))))

;;Obtiene la seleccion de ambos equipos
(define (Seleccion Equipos Fitness Mejores)
  (cond ((null? Fitness)
         Mejores
         ) ;;Crossover
        (else (Seleccion Equipos (cdr Fitness) (append Mejores (list (Seleccion_Equipo (car Equipos) (car Fitness) '())))))))

;;Obtiene la seleccion de un equipo
(define (Seleccion_Equipo Equipo Fitness_Equipo Mejores)
  (cond ((null? Equipo)
         Mejores)
        (else (Seleccion_Equipo (cdr Equipo) (cdr Fitness_Equipo) (append Mejores (list (Seleccion_Individual (car Equipo) (car Fitness_Equipo)))))
        )))

;; Obtiene la seleccion de un tipo de jugador
(define (Seleccion_Individual Jugadores Fitness_Jugadores)
  (seleccionNatural (list Fitness_Jugadores Jugadores)))
  
  
;;**********************************************************************************************************************

;;----------------------------------------------------------------------------------------------------------------------
;;Conjunto de funciones para la primera generación
;;----------------------------------------------------------------------------------------------------------------------
(define (generaEquipos listaFormaciones listaJugadores contador)
(cond ((empty? listaFormaciones) listaJugadores)

      ((equal? contador 1) (generaEquipos (cdr listaFormaciones) (append listaJugadores (list (genera1Aux (car listaFormaciones) '() 1))) (+ contador 1)))
      ((equal? contador 2) (generaEquipos (cdr listaFormaciones) (append listaJugadores (list (genera2Aux (car listaFormaciones) '() 1))) (+ contador 2)))
      (else (generaEquipos (cdr listaFormaciones) (append listaJugadores listaFormaciones) contador))
      )
  )
   
(define (genera1Aux listaFormacion listaJugadores contador)
  (cond ((empty? listaFormacion) (append (generadorPortero '())listaJugadores))
        ((equal? contador 1) (genera1Aux (cdr listaFormacion) (append listaJugadores (generaDefensas1 (car listaFormacion) '())) (+ contador 1)))
        ((equal? contador 2) (genera1Aux (cdr listaFormacion) (append listaJugadores (generaMedios1 (car listaFormacion) '())) (+ contador 1)))
        ((equal? contador 3) (genera1Aux (cdr listaFormacion) (append listaJugadores (generaDelanteros1 (car listaFormacion) '())) contador))
        )
  )
(define (genera2Aux listaFormacion listaJugadores contador)
  (cond ((empty? listaFormacion) (append (generadorPortero2 '())listaJugadores))
        ((equal? contador 1) (genera2Aux (cdr listaFormacion) (append listaJugadores (generaDefensas2 (car listaFormacion) '())) (+ contador 1)))
        ((equal? contador 2) (genera2Aux (cdr listaFormacion) (append listaJugadores (generaMedios2 (car listaFormacion) '())) (+ contador 1)))
        ((equal? contador 3) (genera2Aux (cdr listaFormacion) (append listaJugadores (generaDelanteros2 (car listaFormacion) '() )) contador))
        )
  )
  
;;generador de portero grupo1
(define (generadorPortero caracteristicas)
(append caracteristicas (list (list (random 0 40) (random 180 345))) (list (random 0 10)) (list (random 0 10)) (list 1) (list 0) (list(list 0 0)))
      )


;;generador de defensas grupo1
(define (generaDefensas1 cantidadDefensas listaDefensas)
  (cond ((zero? cantidadDefensas) listaDefensas)
        (else (generaDefensas1 (- cantidadDefensas 1) (append listaDefensas (list (list (list (random 0 274) (random 5 520)))) (list (random 0 10)) (list (random 0 10)) (list (random 1 60)) (list 0) (list(list 0 0)) )


        )

  )))
       
;;generador de medios grupo1

(define (generaMedios1 cantidadMedios listaMedios)
  (cond ((zero? cantidadMedios) listaMedios)
        (else (generaMedios1 (- cantidadMedios 1) (append listaMedios (list (list (list (random 274 510) (random 5 520)))) (list (random 0 10)) (list (random 0 10)) (list (random 1 60)) (list 0) (list(list 0 0)) )
        )
  )))
;;generador de delanteros grupo1
(define (generaDelanteros1 cantidadDelanteros listaDelanteros)
  (cond ((zero? cantidadDelanteros) listaDelanteros)
        (else (generaDelanteros1 (- cantidadDelanteros 1) (append listaDelanteros (list (list (list (random 510 785) (random 5 520)))) (list (random 0 10)) (list (random 0 10)) (list (random 1 60)) (list 0) (list(list 0 0)) )
        )
  )))
;;generador de portero grupo2
(define (generadorPortero2 caracteristicas)
(append caracteristicas (list (list (random 745 785) (random 180 345)) ) (list (random 0 10)) (list (random 0 10)) (list 2) (list 0) (list(list 0 0)))
      )


;;generador de defensas grupo2
(define (generaDefensas2 cantidadDefensas listaDefensas)
  (cond ((zero? cantidadDefensas) listaDefensas)
        (else (generaDefensas2 (- cantidadDefensas 1) (append listaDefensas (list (list (list (random 510 785) (random 5 520)))) (list (random 0 10)) (list (random 0 10)) (list (random 1 60)) (list 0) (list(list 0 0)) )
        )
  )))
;;generador de medios grupo2
(define (generaMedios2 cantidadMedios listaMedios)
  (cond ((zero? cantidadMedios) listaMedios)
        (else (generaMedios2 (- cantidadMedios 1) (append listaMedios (list (list (list (random 274 510) (random 5 520)))) (list (random 0 10)) (list (random 0 10)) (list (random 1 60)) (list 0) (list(list 0 0)) )
        )
  )))
;;generador de delanteros grupo2
(define (generaDelanteros2 cantidadDelanteros listaDelanteros)
  (cond ((zero? cantidadDelanteros) listaDelanteros)
        (else (generaDelanteros2 (- cantidadDelanteros 1) (append listaDelanteros (list (list (list (random 510 785) (random 5 520)))) (list (random 0 10)) (list (random 0 10)) (list (random 1 60)) (list 0) (list(list 0 0)) )
        )
  )))

;; Aqui se llama a la funcion(generaEquipos '((4 4 2) (3 3 4) 10) '() 1)
;;Generador de gen con numeros aleatorios
(define (Random-gen Gen Habilidades)
  (cond ((zero? Habilidades)
         Gen)
        (else
  (Random-gen (cons (random 10) Gen) (- Habilidades 1)))))
;;**********************************************************************************************************************

;;----------------------------------------------------------------------------------------------------------------------
;;Conjunto de funciones que selecciona los individuos más fuertes de cada generacion, con un promedio de sus genes.
;;----------------------------------------------------------------------------------------------------------------------
;;ESTAS SON LAS QUE FUNCIONAN(NUEVAS)

;;Seleccion de jugadores, recibe (listaFtness listaJugadores), retorna (listaMejores)
;;Entrada:(seleccionNatural '( (3 5 6 ) ((1 2 3)(4 5 6) (7 8 9))))-> Retorna: (list (list 4 5 6) (list 7 8 9)) que son las posiciones que tienen mejor fitness
;;                                x x ->mejores fitness

(define (seleccionNatural listaGeneral)
(seleccionMejores (seleccionPorFitness (car listaGeneral) '() 0) (car (cdr listaGeneral)) '() 0 )
 )

;;Entra lista de fitness de alguna posicion (defensas,o medios,o delanteros), retorna una lista con la posicion de los mejores:
;;Ej entra (list 10 3 6 2 5 6 7 )-> retorna (list 0 2 4 5 6)(<- son las posiciones de los jugadores con mas fitness de la lista de Fitness por posicion)

(define (seleccionPorFitness listaFitness listaMejoresFitness contador)
  (cond ((empty? listaFitness) listaMejoresFitness)
        ((>= (car listaFitness) 5) (seleccionPorFitness (cdr listaFitness) (append listaMejoresFitness (list contador)) (+ contador 1)))
        (else (seleccionPorFitness (cdr listaFitness) listaMejoresFitness (+ contador 1)))
        )
  )


(define (seleccionMejores listaMejoresFitness listaJugadores jugadoresMejores contador)
(cond((or (empty? listaJugadores) (empty? listaMejoresFitness)) jugadoresMejores)
     ((equal? (car listaMejoresFitness) contador) (seleccionMejores (cdr listaMejoresFitness) (cdr listaJugadores) (append jugadoresMejores (list(car listaJugadores))) (+ contador 1)))
     (else (seleccionMejores listaMejoresFitness (cdr listaJugadores) jugadoresMejores (+ contador 1)))

     )
  )
;;----------------------------------------------------------------------------------------------------------------------

;;Funcion principal de seleccion de los mejores jugadores.
(define (sleccionPromedioMayor listaJugadores )
  (cond ((empty? listaJugadores) '())
        (else (seleccionAux (promediosGenesJugadores listaJugadores) listaJugadores '()))
        )
)
(define (seleccionAux listaJugadores listaJugadores2 listaMejores)
(cond ((empty? listaJugadores) listaMejores)
      ((>= (car listaJugadores) 5 ) (seleccionAux (cdr listaJugadores) (cdr listaJugadores2)(append listaMejores (car listaJugadores2))))
      (else (seleccionAux (cdr listaJugadores) (cdr listaJugadores2) listaMejores))
      )
  )


;;Hace una lista con el promedio de genes de cada jugador en un equipo
(define (promediosGenesJugadores jugadores)
(promediosGenesAux jugadores '()) 
  )
(define (promediosGenesAux jugadores listaPromediosEquipo )
(cond ((empty? jugadores) (Quicksort listaPromediosEquipo))
      (else (promediosGenesAux (cdr jugadores) (append listaPromediosEquipo (list (promediador (car jugadores))))))      
      )
)


;;Saca el promedio de fitness de cada jugador
(define (promediador jugador)
(promedioAux jugador jugador 0)
)
(define (promedioAux jugador genes promedio)
(cond ((empty? jugador) (/ promedio (contadorDeElementos genes)))
      (else (promedioAux (cdr jugador) genes (+ promedio (car jugador))) )
      )
)
;;**********************************************************************************************************************

;;----------------------------------------------------------------------------------------------------------------------
;;Conjunto de funciones que realiza el cross over de los genes.
;;----------------------------------------------------------------------------------------------------------------------



;;Toma la lista de los mejores jugadores, hace parejas y los mezcla, talque por cada pareja salga un hijo.
(define (crucePrincipal listaMejores)
(cond ((empty? listaMejores) '())
      (else (cruceAux listaMejores '() ))
      )
  )
(define (cruceAux listaDeLosMejores listaNuevosHijos)
(cond ((empty? listaDeLosMejores) listaNuevosHijos)
      ((even? (contadorDeElementos listaDeLosMejores)) (cruceAux (cdr(cdr listaDeLosMejores)) (append listaNuevosHijos (intercambio (car listaDeLosMejores) (car(cdr listaDeLosMejores))))))
      (else (cruceAux '() listaNuevosHijos))
      )

  )

;;Toma dos padres y la mitad de los primeros elementos del padre1 se combinan con los segundos elementos del padre2, creando asi un hijo aun mejor.
(define (intercambio padre1 padre2)
(cond ((or (empty? padre1) (empty? padre2)) '())
      (else (intercambioAux padre1 padre2 '() (quotient (contadorDeElementos padre1) 2) 0))
      )
  )
(define (intercambioAux padre1 padre2 hijo mitadLista contador)
(cond ((equal? mitadLista contador)  ( append hijo padre2))
      (else (intercambioAux (cdr padre1) (cdr padre2) (append hijo (list (car padre1))) mitadLista (+ contador 1)))

      )

  )


;;**********************************************************************************************************************

;;----------------------------------------------------------------------------------------------------------------------
;;Conjunto de funciones que saca fitness para cada individuo
;;----------------------------------------------------------------------------------------------------------------------
;; Evalua la posicion del jugador con respecto a la cancha.


(define (fitnessPortero jugador jugadorNuevo contador balon)
  (cond((empty? jugador) jugadorNuevo)
       ((equal? contador 1) (fitnessPortero (cdr jugador) (append jugadorNuevo (mejorPosPortero (car jugador) '() 1 balon)) (+ contador 1) balon ))
       ((equal? contador 2) (fitnessPortero (cdr jugador) (append jugadorNuevo (list(mejorFuerza (car jugador) 0 balon))) (+ contador 1) balon))
       ((equal? contador 3) (fitnessPortero (cdr jugador) )
       )
  
  ))

(define (mejorPosPortero pos nuevaPos contador balon)
(cond ((empty? pos) nuevaPos)
      ;;Posicion en X
      ((and (> 10 (- (car pos) (car balon))) (or (> 40 (car pos)) (> (car pos) 745) ) (equal? contador 1)) (mejorPosPortero (cdr pos) (append nuevaPos (list (car balon))) (+ contador 1) (cdr balon)))
      ;;Posicion en Y
      ((and (equal? contador 1) (> (car pos) 180) (> 345 (car pos))) (mejorPosPortero (cdr pos) (append nuevaPos (list (car balon))) contador balon ))

      )
  )
(define (mejorFuerza fuerza nuevaFuerza balon)
(cond ((> 10  (car balon) ) (+ nuevaFuerza (- 10 (car balon))))
      (else nuevaFuerza)
      )  
  )

  



(define (evaluadorDefensa canchaX canchaY jugador resultado)
(cond ((and (>= (car canchaY) (car (cdr jugador))) (>= (car (cdr jugador)) (car (cdr canchaY)))) 1)
      (else (fitnessCanchaY jugador))
      )
  )

;;Mejora la posicion del jugador talque tape la cancha propia de posibles goles.
(define (fitnessCanchaY canchaY posicion)
(/ /(- (car canchaY) (car (cdr posicion))) 2)
  )

;;Evalua la posición del jugador con la del contrario para determinar una posición que no le estorbe para un posible gol.
;;Nota: se evalua a jugador 1 y por lo tanto se modifica a jugador 1.
(define (evaluadorAtaque posicionJugador1 posicionJugador2)
(cond ((equal? (car(cdr posicionJugador1)) (car(cdr posicionJugador2))) (fitnessAtaque posicionJugador1 posicionJugador2))
      (else 1)
      )
)

;;Mejora la posicion del jugador con respecto al oponente para lograr meter un gol.
(define (fitnessAtaque jugador1  )
(+ (cdr (car jugador1)) + 1)
)

;;Obtiene el número de jugador
(define (ObtenerCamiseta Jugador)
  (cadddr Jugador))
;;----------------------------------------------------------------------------------------------------------------------
;;Fitness General para el equipo, Bola es la última posición de la bola
;;----------------------------------------------------------------------------------------------------------------------

(define (Fitness_por_equipo Equipo Bola)
  (Fitness_por_equipo_aux Equipo Bola 4 (ObtenerCamiseta (caar Equipo))))


(define (Fitness_por_equipo_aux Equipo Bola Iterador numEquipo)
  (cond ((zero? Iterador)
         '())
        ((equal? Iterador 4)
         (cons (Fit_Portero (caar Equipo) Bola) (Fitness_por_equipo_aux (cdr Equipo) Bola (- Iterador 1) numEquipo)))
        ((equal? Iterador 3)
         (cons (Fit_Defensa (car Equipo) Bola) (Fitness_por_equipo_aux (cdr Equipo) Bola (- Iterador 1) numEquipo)))
        ((equal? Iterador 2)
         (cons (Fit_Medios (car Equipo) Bola numEquipo) (Fitness_por_equipo_aux (cdr Equipo) Bola (- Iterador 1) numEquipo)))
        ((equal? Iterador 1)
         (cons (Fit_Delanteros (car Equipo) Bola) (Fitness_por_equipo_aux (cdr Equipo) Bola (- Iterador 1) numEquipo))) 
        ))
         
;;----------------------------------------------------------------------------------------------------------------------
;;Fitness para porteros
;;----------------------------------------------------------------------------------------------------------------------       
;; Fitness para todos los porteros
(define (Fit_Portero Portero Bola)
  (cond ((or (<= (car Bola) 274) (>= (car Bola) 510)) ;; Valida que es el izquierdo o izquierdo
        (list (+ (* (Nota_a_bola Portero Bola 436.6) 0.7 ) (* (cadr Portero) 0.3))))
        (else (list (* (/ (- 520 (abs (- (cadar Portero) (cadr Bola)))) 520) 10)))))
                
;;----------------------------------------------------------------------------------------------------------------------
;;Fitness para defensas
;;----------------------------------------------------------------------------------------------------------------------
;; Fitness para todos los defensas, retorna lista con notas de los defensas
(define (Fit_Defensa Defensas Bola)
  (cond ((null? Defensas)
         '())
        (else (cons (Fit_Defensa_individual (car Defensas) Bola) (Fit_Defensa (cdr Defensas) Bola)))))


;; Fitness para cada defensa
(define (Fit_Defensa_individual Defensa Bola)
  (cond ((and (>= (car Bola) 0) (<= (car Bola) 274) (<= (caar Defensa) 274)) ;; Evalua que la bola está en el area de defensa izquierda
         (cond ((> (- (caar Defensa) (car Bola)) 0) ;; Bola detrás de la defensa
                (+ (* (cadr Defensa) 0.5) (* (caddr Defensa) 0.2))) ;; Por estar detras de los defensas entonces la nota de 10 le baja a 7
               (else (calcularNotaporDelante Defensa Bola))))
        ((and (>= (car Bola) 510) (<= (car Bola) 785) (>= (caar Defensa) 510)) ;; Evalua que la bola está en el area de defensa derecha
         (cond ((< (- (caar Defensa) (car Bola)) 0) ;; Bola detrás de la defensa
                (+ (* (cadr Defensa) 0.5) (* (caddr Defensa) 0.2))) ;; Por estar detras de los defensas entonces la nota de 10 le baja a 7
               (else (calcularNotaporDelante Defensa Bola))))
        (else 
               (cond ((<= (caar Defensa) 274) ;; Valida que es defensa de la izquierda
                      (Nota_a_linea Defensa Bola 236 40)) 
                     (else (Nota_a_linea Defensa Bola 236 745))))
        )
  )


;; Obtiene la nota cuando la bola está por delante
(define (calcularNotaporDelante Defensa Bola)
  (* (/ (- 583.3 (Magnitud (car Defensa) Bola)) 583.3) 10))

;;----------------------------------------------------------------------------------------------------------------------
;;Fitness medios
;;----------------------------------------------------------------------------------------------------------------------
;; Fitness para todos los medios
(define (Fit_Medios Medios Bola numEquipo)
(cond ((null? Medios)
         '())
        (else (cons (Fit_Medios_individual (car Medios) Bola numEquipo) (Fit_Medios (cdr Medios) Bola numEquipo)))))
 
;;Fitness para cada medio
(define (Fit_Medios_individual Medio Bola numEquipo)
    (cond ((equal? numEquipo 1) ;;Equipo izquierdo
      (Medios_Izquierdos Medio Bola))
          (else (Medios_Derechos Medio Bola))))
         
        


;;Evaluador de medios en la izquierda
(define (Medios_Izquierdos Medio Bola)
  (cond ((and (>= (car Bola) 274) (<= (car Bola) 510))
         (cond ((> (- (caar Medio) (car Bola)) 0) ;; Pierde 30% de la nota
                (+ (* (Nota_a_bola Medio Bola 566.5) 0.5) (* (Nota_a_linea Medio Bola 236 274) 0.2)))
               (else
                (+ (* (Nota_a_bola Medio Bola 566.5) 0.4) (* (Nota_a_linea Medio Bola 236 510) 0.4) (* (cadr Medio) 0.2))
                )))
        (else (Nota_a_linea Medio Bola 236 (car Bola)))))


;; Evaluador de medios por la derecha
(define (Medios_Derechos Medio Bola)
  (cond ((and (>= (car Bola) 274) (<= (car Bola) 510))
         (cond ((< (- (caar Medio) (car Bola)) 0) ;; Pierde 30% de la nota
                (+ (* (Nota_a_bola Medio Bola 566.5) 0.5) (* (Nota_a_linea Medio Bola 236 510) 0.2)))
               (else
                (+ (* (Nota_a_bola Medio Bola 565.5) 0.4) (* (Nota_a_linea Medio Bola 236 274) 0.4) (* (cadr Medio) 0.2))
                )))
        (else (Nota_a_linea Medio Bola 236 (car Bola)))))


;;----------------------------------------------------------------------------------------------------------------------
;;Fitness Delanteros
;;----------------------------------------------------------------------------------------------------------------------
(define (Fit_Delanteros Delanteros Bola)
  (cond ((null? Delanteros)
         '())
        (else (cons (Fit_Delanteros_individual (car Delanteros) Bola) (Fit_Delanteros (cdr Delanteros) Bola)))))

(define (Fit_Delanteros_individual Delantero Bola)
  (cond ((and (>= (car Bola) 510) (<= (car Bola) 785) (>= (caar Delantero) 510)) ;;Delanteros del equipo de la izquierda
         (cond ((> (- (caar Delantero) (car Bola)) 0)
                (+ (* (Nota_a_bola Delantero Bola 583.8) 0.4) (* (+ (/ (- 4373 (* (Magnitud Bola '(785 262)) (cadr Delantero))) 4373)
                                                                    (/ (- 583.8 (Magnitud Bola (car Delantero))) 583.8)) 5)))
               (else (* (/ (- 4373 (* (Magnitud Bola '(785 262)) (cadr Delantero))) 4373) 10))))
         ((and (>= (car Bola) 0) (<= (car Bola) 274) (<= (caar Delantero) 274)) ;; Delanteros del equipo de la derecha
          (cond ((< (- (caar Delantero) (car Bola)) 0)
                (+ (* (Nota_a_bola Delantero Bola 583.8) 0.4) (* (+ (/ (- 4373 (* (Magnitud Bola '(0 262)) (cadr Delantero))) 4373)
                                                                    (/ (- 583.8 (Magnitud Bola (car Delantero))) 583.8)) 5)))
                (else (* (/ (- 4373 (* (Magnitud Bola '(785 262)) (cadr Delantero))) 4373) 10))))
         (else (cond ((>= (caar Delantero) 510)
          (* (/ (- 437.3 (Magnitud (car Delantero) '(785 262))) 437.3) 10))
                     (else (* (/ (- 437.3 (Magnitud (car Delantero) '(0 262))) 437.3) 10))))))
         
   
;;Calcula la nota, según cuanto dura en llegar al balon
(define (Nota_a_bola Jugador Bola Mayor_tiempo)
  (* (/ (- Mayor_tiempo (/ (Magnitud (car Jugador) Bola) (caddr Jugador))) Mayor_tiempo) 10))

;;Calcula la nota en llegar a la linea
(define (Nota_a_linea Jugador Bola Mayor_tiempo Linea)
  (* (/ (- Mayor_tiempo (/ (abs (- (caar Jugador) Linea)) (caddr Jugador))) Mayor_tiempo) 10))
         
;;Obtiene la diagonal entre dos pares ordenados
(define (Magnitud Par1 Par2)
  (inexact->exact
   (sqrt (+ (expt (- (car Par1) (car Par2)) 2) (expt (- (cadr Par1) (cadr Par2)) 2)))
   ))

;; Obtiene la diagonal
(define (Diagonal Par1)
(inexact->exact
   (sqrt (+ (expt (car Par1) 2) (expt (cadr Par1) 2)))
   ))

;; Variables de prueba
(define Portero '(((20 262) 6 7 1 4 (274 250))))
(define Defensas '(((120 160) 7 6 10 4 (274 250)) ((120 250) 2 4 22 4 (274 250)) ((120 320) 7 9 15 4 (274 250)) ((120 450) 8 6 3 4 (274 250))))
(define Medios '(((350 160) 4 2 2 4 (274 250)) ((350 250) 7 1 22 4 (274 250)) ((350 320) 2 1 13 4 (274 250)) ((350 450) 9 8 9 4 (274 250))))
(define Delanteros '(((520 250) 10 10 10 4 (274 250)) ((520 450) 8 5 16 4 (274 250))))
(define Equipo1 (list Portero Defensas Medios Delanteros))