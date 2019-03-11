;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Proyecto Lenguajes|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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
;;Conjunto de funciones para la primera generación
;;----------------------------------------------------------------------------------------------------------------------
;;Generador de gen con numeros aleatorios
(define (Random-gen Gen Habilidades)
  (cond ((zero? Habilidades)
         Gen)
        (else
  (Random-gen (cons (random 10) Gen) (- Habilidades 1)))))
;;**********************************************************************************************************************

;;----------------------------------------------------------------------------------------------------------------------
;;Fitness para defensas
;;----------------------------------------------------------------------------------------------------------------------
;;**********************************************************************************************************************
;; Fitness para defensa
(define (Fit_Defensa_individual Defensa Bola)
  (cond ((and (>= (car Bola) 0) (<= (car Bola) 274)) ;; Evalua que la bola está en el area de defensa izquierda
         (cond ((> (- (caar Defensa) (car Bola)) 0) ;; Bola detrás de la defensa
                (+ (* (cadr Defensa) 0.5) (* (caddr Defensa) 0.2))) ;; Por estar detras de los defensas entonces la nota de 10 le baja a 70
               (else (calcularNotaporDelante Defensa Bola))))
        ((and (>= (car Bola) 510) (<= (car Bola) 785)) ;; Evalua que la bola está en el area de defensa derecha
         (cond ((< (- (caar Defensa) (car Bola)) 0) ;; Bola detrás de la defensa
                (+ (* (cadr Defensa) 0.5) (* (caddr Defensa) 0.2))) ;; Por estar detras de los defensas entonces la nota de 10 le baja a 70
               (else (calcularNotaporDelante Defensa Bola))))
        (else 
               (cond ((<= (caar Defensa) 274) ;; Valida que es defensa de la izquierda
                      (* (/ (abs (- (caar Defensa) 40)) 210) 10)) 
                     (else (* (/ (abs (- (caar Defensa) 40 510)) 210) 10))))
        )
  )


;; Obtiene la nota cuando la bola es
(define (calcularNotaporDelante Defensa Bola)
  (* (+ (/ (abs (- (caar Defensa) (car Bola))) 274)
        (/ (abs (- (cadar Defensa) (cadr Bola))) 515)) 5))

;;----------------------------------------------------------------------------------------------------------------------
;;Conjunto de funciones que selecciona los individuos más fuertes de cada generacion, con un promedio de sus genes.
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





