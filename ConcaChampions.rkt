#lang racket
(require graphics/graphics)
(require racket/math)
(open-graphics)

;;----------------------------------------------------------------------------------------------------------------------
;;Lógica
;;----------------------------------------------------------------------------------------------------------------------
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
  (cond ((null? Equipos)
         '())
        (else (cons (Reproducir (car Equipos) Bola) (Genética (cdr Equipos) Bola))))
  )


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
        ((equal? contador 1) (genera2Aux (cdr listaFormacion) (append listaJugadores (list (generaDefensas2 (car listaFormacion) '()))) (+ contador 1)))
        ((equal? contador 2) (genera2Aux (cdr listaFormacion) (append listaJugadores (list (generaMedios2 (car listaFormacion) '()))) (+ contador 1)))
        ((equal? contador 3) (genera2Aux (cdr listaFormacion) (append listaJugadores (list (generaDelanteros2 (car listaFormacion) '()))) contador))
        )
  )
  
;;generador de portero grupo1
(define (generadorPortero caracteristicas)
(list (append caracteristicas (list (list (random 0 40) (random 180 345)) (random 0 10) (random 0 10) 1 0 (list 0 0))))
      )


;;generador de defensas grupo1
(define (generaDefensas1 cantidadDefensas listaDefensas)
  (cond ((zero? cantidadDefensas) (list listaDefensas))
        (else (generaDefensas1 (- cantidadDefensas 1) (append listaDefensas (list (list (list (random 0 274) (random 5 520)) (random 0 10) (random 0 10) (random 1 60)  0 (list 0 0) )))


        )

  )))
       
;;generador de medios grupo1

(define (generaMedios1 cantidadMedios listaMedios)
  (cond ((zero? cantidadMedios) listaMedios)
        (else (generaMedios1 (- cantidadMedios 1) (append listaMedios (list (list  (list (random 274 510) (random 5 520)) (random 0 10) (random 0 10) (random 1 60) 0 (list 0 0) )))
        )
  )))
;;generador de delanteros grupo1
(define (generaDelanteros1 cantidadDelanteros listaDelanteros)
  (cond ((zero? cantidadDelanteros) listaDelanteros)
        (else (generaDelanteros1 (- cantidadDelanteros 1) (append listaDelanteros (list (list (list (random 510 785) (random 5 520)) (random 0 10) (random 0 10) (random 1 60) 0 (list 0 0))))
        )
  )))
;;generador de portero grupo2
(define (generadorPortero2 caracteristicas)
(append caracteristicas (list (list (list (random 745 785) (random 180 345))  (random 0 10) (random 0 10) 2 0 (list 0 0))))
      )


;;generador de defensas grupo2
(define (generaDefensas2 cantidadDefensas listaDefensas)
  (cond ((zero? cantidadDefensas) listaDefensas)
        (else (generaDefensas2 (- cantidadDefensas 1) (append listaDefensas (list (list (list (random 510 785) (random 5 520)) (random 0 10) (random 0 10)  (random 1 60) 0 (list 0 0) )))
        )
  )))
;;generador de medios grupo2
(define (generaMedios2 cantidadMedios listaMedios)
  (cond ((zero? cantidadMedios) listaMedios)
        (else (generaMedios2 (- cantidadMedios 1) (append listaMedios (list (list (list (random 274 510) (random 5 520)) (random 0 10)  (random 0 10) (random 1 60) 0 (list 0 0)) ))
        )
  )))
;;generador de delanteros grupo2
(define (generaDelanteros2 cantidadDelanteros listaDelanteros)
  (cond ((zero? cantidadDelanteros) listaDelanteros)
        (else (generaDelanteros2 (- cantidadDelanteros 1) (append listaDelanteros (list (list (list (random 510 785) (random 5 520)) (random 0 10) (random 0 10) (random 1 60) 0 (list 0 0)) ))
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
;;Conjunto de funciones que saca fitness para cada individuo
;;----------------------------------------------------------------------------------------------------------------------
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
                (+ (* (Nota_a_bola Delantero Bola 583.8) 0.4) (* (* (+ (/ (- 4373 (* (Magnitud Bola '(785 262)) (cadr Delantero))) 4373)
                                                                    (/ (- 583.8 (Magnitud Bola (car Delantero))) 583.8)) 5) 0.6)))
               (else (* (/ (- 4373 (* (Magnitud Bola '(785 262)) (cadr Delantero))) 4373) 10))))
         ((and (>= (car Bola) 0) (<= (car Bola) 274) (<= (caar Delantero) 274)) ;; Delanteros del equipo de la derecha
          (cond ((< (- (caar Delantero) (car Bola)) 0)
                (+ (* (Nota_a_bola Delantero Bola 583.8) 0.4) (* (* (+ (/ (- 4373 (* (Magnitud Bola '(0 262)) (cadr Delantero))) 4373)
                                                                    (/ (- 583.8 (Magnitud Bola (car Delantero))) 583.8)) 5) 0.6)))
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

;;----------------------------------------------------------------------------------------------------------------------
;;Mutacion de genes
;;----------------------------------------------------------------------------------------------------------------------
;;Función mutacion
;;Entra lista de mejores y le cambia un gen aleatoriamente
;;Cambiará fuerza o velocidad aleatoriamente.
;;Entra listaMejores, sale una lista con algun jugador mutado

(define (mutacion listaMejores )
(mutacionAux listaMejores '() 1 (random 1 11) (random 2 3) )
 )

(define (mutacionAux listaMejores listaJugadoresMasMutado contador contador1Rand contador2Rand)

(cond ((empty? listaMejores) listaJugadoresMasMutado)
      ((equal? contador contador1Rand) (mutacionAux (cdr listaMejores) (append listaJugadoresMasMutado (list (mutadorJugador (car listaMejores) '() 1 contador2Rand))) (+ contador 1) contador1Rand contador2Rand))
      (else (mutacionAux (cdr listaMejores) (append listaJugadoresMasMutado (list (car listaMejores))) (+ contador 1) contador1Rand contador2Rand))
      )
  )
  
 
(define (mutadorJugador jugador jugadorMutado contador contadorRand)
(cond ((empty? jugador) jugadorMutado)
      ((equal? contador contadorRand) ( mutadorJugador (cdr jugador) (append jugadorMutado (list (random 0 10))) (+ contador 1) contadorRand) )
      (else (mutadorJugador (cdr jugador) (append jugadorMutado (list (car jugador))) (+ contador 1) contadorRand))
      )
  )
;;-------------------------------------------------------------------------------------------------------------------------------------------------------------
(define (reproduccion mejoresJugadores cantidadJugadores)
(reproduccionAux mejoresJugadores '() cantidadJugadores (- cantidadJugadores (contadorDeElementos mejoresJugadores)))
 )


(define (reproduccionAux mejoresJugadores listaDeNuevosJugadores cantidadJugadores hijosPorCrear)
(cond((zero? hijosPorCrear) (append listaDeNuevosJugadores mejoresJugadores))
     (else (reproduccionAux (cdr mejoresJugadores) (append  listaDeNuevosJugadores (parejas (car mejoresJugadores) (cdr(car mejoresJugadores)))) cantidadJugadores (- hijosPorCrear 1) ))

     )
 )

;; Realiza el cruce entre 2 padres
(define (parejas padre1 padre2)
  (list (Generar_Gen padre1 padre2)))

;; Obtiene el gen del nuevo hijo
(define (Generar_Gen Padre1 Padre2)
  (list (list (Promedio (caar Padre1) (caar Padre2)) (Promedio (cadar Padre1) (cadar Padre2)))
        (Promedio (cadr Padre1) (cadr Padre2))
        (Promedio (caddr Padre1) (caddr Padre2))
        (random 20 51)
        0
        (cadr(cddr(cddr Padre1)))))
        
;; Funcion general para obtener un valor según posicion
(define (Obtener_Posicion Lista Posición)
  (Obtener_aux Lista Posición 0)
  )

;; Funcion auxiliar que obtiene el valor hasta llegar al iterador correcto
(define (Obtener_aux Lista Posicion Iterador)
  (cond ((equal? Posicion Iterador)
         (car Lista))
        (else (Obtener_aux (cdr Lista) Posicion (+ Iterador 1)))))

;;----------------------------------------------------------------------------------------------------------------------
;;Fitness y reproduccion
;;----------------------------------------------------------------------------------------------------------------------
;;Función Reproduccion
;;Entra lista de un Equipo 
;;Tendrá un nuevo hijo cada tipo, y cada uno tendrá los nuevos fitness


;; Obtiene un equipo nuevo, donde ya se reproducen o se realiza el crossover, por lo que se realiza lo siguiente
(define (Reproducir Equipo Bola)
   (append (list (car (Insertar_en_fit Equipo (Fitness_por_equipo Equipo Bola) '()))) ;; Quito el portero, debido a que este solo mutara, se añade su funcion de fitness
                             (Reproducir_Equipo (cdr (Insertar_en_fit Equipo (Fitness_por_equipo Equipo Bola) '())) '()))) ;; Envio el resto del equipo a reproducirse según su tipo, con el nuevo fitness cada uno


;; Obtiene una parte del equipo (sin portero), reproducida con el nuevo fitness anteriormente asignado
(define (Reproducir_Equipo Jugadores_con_F Nueva)
  (cond ((null? Jugadores_con_F)
         Nueva)
        (else (Reproducir_Equipo (cdr Jugadores_con_F) (append Nueva (list (Reproducir_2 (car Jugadores_con_F))))))))


;; Reproducirá un tipo de jugador                              
(define (Reproducir_2 Jugadores)
  (cond ((equal?  (contadorDeElementos Jugadores) 2)
         (append (list (car Jugadores)) (parejas (car (Mayores_Fitness Jugadores)) (cadr (Mayores_Fitness Jugadores)))))
        (else (append (Mayores_Fitness Jugadores) (parejas (car (Mayores_Fitness Jugadores)) (cadr (Mayores_Fitness Jugadores)))))))

;; Obtiene los mayores fitness de n-1 (FUNCION BASICA)
(define (Mayores_Fitness Jugadores)
  (cond ((equal?  (contadorDeElementos Jugadores) 2) ;;Valida si solo son dos genes
         (Mayores_aux Jugadores 2 '()))
        (else (Mayores_aux Jugadores (- (contadorDeElementos Jugadores) 1) '()))))

;; Se usa para obtener la lista de mayores (FUNCION BASICA)
(define (Mayores_aux Jugadores Cantidad Mayores)
  (cond ((zero? Cantidad)
         Mayores)
        (else (Mayores_aux (cadr(Mayor_lista (cdr Jugadores) (car Jugadores) '()))  (- Cantidad 1) (append Mayores (list (car(Mayor_lista (cdr Jugadores) (car Jugadores) '()))))))))

;; Obtiene la lista mayor según el valor de fitness (FUNCION BASICA)
(define (Mayor_lista Jugadores Mayor Resto)
  (cond ((null? Jugadores)
         (list Mayor Resto))
        ((> (Obtener_Posicion (car Jugadores) 4) (Obtener_Posicion Mayor 4))
         (Mayor_lista (cdr Jugadores) (car Jugadores) (cons Mayor Resto)))
        (else (Mayor_lista (cdr Jugadores) Mayor (cons (car Jugadores) Resto))
        )))

;;Inserta el valor del nuevo fitness en el antiguo 
(define (Insertar_en_fit Equipo Fitness Final) ;; Final vacia
  (cond ((null? Equipo)
         Final)
        (else (Insertar_en_fit (cdr Equipo) (cdr Fitness) (append Final (list (Insertar_tipo (car Equipo) (car Fitness) '())) )))))
         
;;Inserta en un solo tipo de jugador el fitness    
(define (Insertar_tipo Jugadores Fit_jugadores Parcial)
  (cond ((null? Jugadores)
         Parcial)
        (else (Insertar_tipo (cdr Jugadores) (cdr Fit_jugadores) (append Parcial (list (Insertar_individual (car Jugadores) (car Fit_jugadores) '() 0)))))))
        
;;Inserta en un gen el fitness
(define (Insertar_individual Gen Fitness Parcial Iterador)
  (cond ((null? Gen)
         Parcial)
        ((equal? Iterador 4)
         (Insertar_individual (cdr Gen) Fitness (append Parcial (list Fitness)) (+ Iterador 1)))
        (else (Insertar_individual (cdr Gen) Fitness (append Parcial (list (car Gen))) (+ Iterador 1)))))

;; Promedio entre 2 numeros
(define (Promedio Num1 Num2)
  (/ (+ Num1 Num2) 2))
;;----------------------------------------------------------------------------------------------------------------------

;; Variables de prueba
(define Portero '(((20 262) 6 7 1 4 (274 250))))
(define Defensas '(((120 160) 7 6 10 4 (274 250)) ((120 250) 2 4 22 4 (274 250)) ((120 320) 7 9 15 4 (274 250)) ((120 450) 8 6 3 4 (274 250))))
(define Medios '(((350 160) 4 2 2 4 (274 250)) ((350 250) 7 1 22 4 (274 250)) ((350 320) 2 1 13 4 (274 250)) ((350 450) 9 8 9 4 (274 250))))
(define Delanteros  '(((350 160) 4 2 2 4 (274 250)) ((350 250) 7 1 22 4 (274 250))))
(define Equipo_Lista (list Portero Defensas Medios Delanteros))


(cadar (Genética (list Equipo_Lista Equipo_Lista) '(120 260)))
(cadar (Genética (Genética (list Equipo_Lista Equipo_Lista) '(120 260)) '(260 355)))
(cadar (Genética (Genética (Genética (list Equipo_Lista Equipo_Lista) '(120 260)) '(260 355)) '(200 300)))



;;-----------------------------------------------------------------------------------------------------------------------------
;;                                               INTERFAZ
;;-----------------------------------------------------------------------------------------------------------------------------
;;INICIO DEL JUEGO
;(define Equipos_Generados ((list Primera_Generacion '(4 4 2) '(3 5 2) 10)))
(define (inicioDelJuego)
  (begin
    (dibujarCancha)
    ;(mostrarDatos '(1 0) 12)
    
    ;(movimientoDeJugadores jugadores 0 '() (DiagonaldelJugador (car jugadores)) (AnguloJugador (car jugadores)) balon)
   
    ;(Graficar_Equipo (car Equipos_Generados) (cadr Equipos_Generados) '(200 100))
    (mostrarDatos '(1 1) 17)
    (copy-viewport ventana2 ventana1)
    ((clear-viewport ventana2))
    ))

;;VENTANAS
(define ventana1 (open-viewport "*ConcaChampionsCE*" 800 580))
(define ventana2 (open-pixmap "*ConcaChampionsCE*" 800 540))
;;                     Delantero A                  D.Central Inferior A       Lateral Inferior A       Lateral Superior A           PORTERO A             D.Central Superior A        Medio Superior A      Medio Central Superior A       Medio Central  A        Medio Central Inferior  A     Medio Inferior  A       
(define jugadores '( ((350 250) (700 400) 16 2) ((70 300) (700 400) 16 2) ((70 400) (300 300) 10 3) ((70 100) (455 260) 20 1) ((10 250) (700 400) 16 2) ((70 200) (700 400) 16 2) ((200 70) (700 400) 16 2) ((210 150) (700 400) 16 2)  ((230 250) (700 400) 16 2) ((210 350) (700 400) 16 2) ((200 440) (700 400) 16 2))) ;; LISTA DE JUGADORES

;;                     Delantero M                  D.Central Inferior M       Lateral Inferior M       Lateral Superior M           PORTERO M             D.Central Superior M        Medio Superior M      Medio Central Superior M       Medio Central  M        Medio Central Inferior  M     Medio Inferior  M                        
(define jugadoresM '( ((450 250) (700 400) 16 2) ((458 300) (700 400) 16 2) ((100 52) (300 300) 10 3) ((360 90) (455 260) 20 1) ((20 230 250) (700 400) 16 2) ((70 200) (700 400) 16 2) ((200 70) (700 400) 16 2) ((210 150) (700 400) 16 2)  ((230 250) (700 400) 16 2) ((210 350) (700 400) 16 2) ((200 440) (700 400) 16 2))) ;; LISTA DE JUGADORES M

(define balon '((400 200) 0 45))


;;Mostrando el Marcador y generaciones
(define (mostrarDatos marcador generacion)
  (begin
    ((draw-string ventana2) (make-posn 430 600) (number->string (car marcador)) "white")
    ((draw-string ventana2) (make-posn 490 600) (number->string (cadr marcador)) "white")
    ((draw-string ventana2) (make-posn 600 650) "Generacion: " "black")
    ;;ACÁ ABAJO SE LE PASA EL PARAMETRO DE LA GENERACION EN LA QUE QUEDÓ
    ;;((draw-string window2) (make-posn 820 615) (number->string generacion) "white")
    ))

;;Dibujando la Cancha
(define (dibujarCancha)
  (begin
    ;;((draw-solid-rectangle ventana2) (make-posn 0 0) 800 700 "Dark Gray") ;;Background
    ((draw-solid-rectangle ventana2) (make-posn 0 0) 800 540 "Forest Green") ;;Pasto
    ((draw-solid-rectangle ventana2) (make-posn 0 0) 5 540 "black")   ;;Linea vertical
    ((draw-solid-rectangle ventana2) (make-posn 795 0) 5 540 "black") ;;Linea vertical
    ((draw-solid-rectangle ventana2) (make-posn 0 0) 920 5 "black")   ;;Linea horizontal
    ((draw-solid-rectangle ventana2) (make-posn 0 535) 920 5 "black") ;;Linea horizontal
    ((draw-solid-rectangle ventana2) (make-posn 405 0) 5 540 "black") ;;Linea medio campo
    ((draw-solid-ellipse ventana2) (make-posn 400 262) 15 15 "black") ;;Circulo de posicion de la Balon
    ((flip-ellipse ventana2) (make-posn 333 195) 150 150 "black")     ;;Circulo central
    ((flip-rectangle ventana2) (make-posn -130 180) 180 180 "black")   ;;Area equipo izquierdo
    
    ;;((flip-rectangle ventana2) (make-posn -100 210) 120 120 "white")

    ;;Marco equipo izquierdo
    ((draw-solid-rectangle ventana2) (make-posn 18 210) 2 120 "white") ;;Linea vertical
    ((draw-solid-rectangle ventana2) (make-posn 0 210) 20 2 "white") ;;Linea horizontal
    ((draw-solid-rectangle ventana2) (make-posn 0 328) 20 2 "white") ;;Linea horizontal


    
    ((flip-rectangle ventana2) (make-posn 750 180) 180 180 "black")   ;;Area equipo derecho
    ;;((flip-rectangle ventana2) (make-posn 780 210) 120 120 "white")

    ;;Marco equipo izquierdo
    ((draw-solid-rectangle ventana2) (make-posn 780 210) 2 120 "white") ;;Linea vertical
    ((draw-solid-rectangle ventana2) (make-posn 780 210) 20 2 "white") ;;Linea horizontal
    ((draw-solid-rectangle ventana2) (make-posn 780 328) 20 2 "white") ;;Linea horizontal

    
    ))


;;DATOS DE LOS JUGADORES


(define (MovimientoFinal jugador)
  (cons (list (posicionFinalEnX  jugador) (posicionFinalEnY jugador)) (cdr jugador)))

(define (posicionEnX jugador)
  (caar jugador))

(define (posicionEnY jugador)
  (cadar jugador))

(define (posicionFinalEnX jugador)
  (caadr jugador))

(define (posicionFinalEnY jugador)
  (car (cdadr jugador)))


(define (DiagonaldelJugador jugador) ;;Diagonal del jugador segun la posicion en X y en Y
  (sqrt (+ (expt (- (posicionFinalEnX  jugador) (posicionEnX jugador)) 2)
          (expt (- (posicionFinalEnY jugador) (posicionEnY jugador)) 2)))
  )


(define (obtenerSiguienteJugador jugadores)
  (cond ((null? (cdr jugadores))
         (car jugadores))
        (else (cadr jugadores))
        ))

(define (pasosEnY jugadores step diagonal angulo)
  (+ (posicionEnY (car jugadores)) (* step (sin (degrees->radians angulo))))
  )


(define (pasosEnX jugadores step diagonal angulo)
  (+ (posicionEnX (car jugadores)) (* step (cos (degrees->radians angulo))))
  )


(define (pendiente jugador)
  (cond ((zero? (- (posicionFinalEnX  jugador) (posicionEnX jugador)))
         (/ (- (posicionFinalEnY jugador) (posicionEnY jugador)) 0.5))
        (else (/ (- (posicionFinalEnY jugador) (posicionEnY jugador)) (- (posicionFinalEnX  jugador) (posicionEnX jugador))))
        )
  )

(define (AnguloJugadorAux jugador)
  (list (- (posicionFinalEnY jugador) (posicionEnY jugador)) (- (posicionFinalEnX  jugador) (posicionEnX jugador))))

(define (AnguloJugador jugador);;obtiene el angulo del jugador
  (cond
    ((and (< (car (AnguloJugadorAux jugador)) 0) (< (cadr (AnguloJugadorAux jugador)) 0))
     (- (radians->degrees (atan (pendiente jugador))) 180)
     )
    ((and (> (car (AnguloJugadorAux jugador)) 0) (< (cadr (AnguloJugadorAux jugador)) 0))
     (+ (radians->degrees (atan (pendiente jugador))) 180)
     )
    (else (radians->degrees (atan (pendiente jugador))))
    )
  )

(define (dibujarJugadores jugadores number)  
  (cond ((null? jugadores) #t)
        (else (begin
                ((draw-solid-rectangle ventana2) (make-posn (posicionEnX (car jugadores)) (posicionEnY (car jugadores))) 30 30 "yellow")
                ((draw-string ventana2) (make-posn (+ (caaar jugadores) 13) (+ (cadar (car jugadores)) 15)) (number->string number) "black")
                (dibujarJugadores (cdr jugadores) (+ 1 number))
                ))
        )
  )

(define (dibujarJugadoresM jugadoresM number)  
  (cond ((null? jugadoresM) #t)
        (else (begin
                ((draw-solid-rectangle ventana2) (make-posn (posicionEnX (car jugadores)) (posicionEnY (car jugadoresM))) 30 30 "purple")
                ((draw-string ventana2) (make-posn (+ (caaar jugadoresM) 13) (+ (cadar (car jugadoresM)) 15)) (number->string number) "black")
                (dibujarJugadoresM (cdr jugadoresM) (+ 1 number))
                ))
        )
  )





(define (fuerzaPatada jugador);;Fuerza con la que patea
  (caddr jugador)
  )




(define (interseccionBalon jugador xm ym Balon jugadores)
  (cond ((and (< xm (+ (posicionEnX Balon) 20)) (< ym (+ (posicionEnY Balon) 20)) (> (+ xm 30) (posicionEnX Balon)) (> (+ ym 30) (posicionEnY Balon)))
         (begin
           (display (AnguloJugador (list (car Balon) (list 920 260)))) (newline)
           (list (car Balon) (fuerzaPatada jugador) 50)
           ))
        (else (interseccionBalonAux jugadores Balon))
        )
  )

(define (interseccionBalonAux jugadores Balon)
  (cond ((null? jugadores) Balon)
        ((and (< (posicionEnX (car jugadores)) (+ (posicionEnX Balon) 20)) (< (posicionEnY (car jugadores)) (+ (posicionEnY Balon) 20))
              (> (+ (posicionEnX (car jugadores)) 30) (posicionEnX Balon)) (> (+ (posicionEnY (car jugadores)) 30) (posicionEnY Balon)))  ;;angulo del jugador
         (list (car Balon) (fuerzaPatada (car jugadores)) 50)
         )
        (else (interseccionBalonAux (cdr jugadores) Balon)) 
        )
  )


(define (dibujarVentana)
  (begin
    (copy-viewport ventana2 ventana1)
    ((clear-viewport ventana2))
    )
  )

;;DATOS DEL BALON

(define (anguloDelBalon Balon)
  (caddr Balon))

(define (fuerzaDelBalon Balon)
  (cadr Balon))

                                          
(define (validarLimites Balon)
  (cond ((< (posicionEnY Balon) 1)
         (list (list (posicionEnX Balon) 1) (fuerzaDelBalon Balon) (+ (anguloDelBalon Balon) 240)))
        ((> (posicionEnY Balon) 519)
         (list (list (posicionEnX Balon) 519) (fuerzaDelBalon Balon) (- (anguloDelBalon Balon) 240)))
        ((and (< (posicionEnX Balon) 1) (or (< (posicionEnY Balon) 165) (> (posicionEnY Balon) 360)))
         (list (list 1 (posicionEnY Balon)) (fuerzaDelBalon Balon) (+ (anguloDelBalon Balon) 240)))
        ((and (> (posicionEnX Balon) 800) (or (< (posicionEnY Balon) 165) (> (posicionEnY Balon) 360))) ;;Esta linea es para los rebotes de la bola en X
         (list (list 800 (posicionEnY Balon)) (fuerzaDelBalon Balon) (+ (anguloDelBalon Balon) 240))) ;;Esta linea es para los rebotes de la bola en Y
        (else Balon)
    ))

;;Acá se puede variar la duración del tiro, es decir de la Balon andando

(define (cambiarFuerzaBalon Balon)
    (cond ((> (fuerzaDelBalon Balon) 0)
           (validarLimites (list
                             (list (+ (posicionEnX Balon) (* (fuerzaDelBalon Balon) (cos (degrees->radians (anguloDelBalon Balon)))))
                                   (+ (posicionEnY Balon) (* (fuerzaDelBalon Balon) (sin (degrees->radians (anguloDelBalon Balon))))))
                             (- (fuerzaDelBalon Balon) 0.38) (anguloDelBalon Balon))))
          (else (list (car Balon) (fuerzaDelBalon Balon) (anguloDelBalon Balon)))
          )
    
  )

(define (dibujarBalon Balon)
  (cond ((or (< (fuerzaDelBalon Balon) 0) (zero? (fuerzaDelBalon Balon)))
         (begin
           ((draw-solid-ellipse ventana2) (make-posn (posicionEnX Balon) (posicionEnY Balon)) 20 20 "white")
           ((draw-ellipse ventana2) (make-posn (posicionEnX Balon) (posicionEnY Balon)) 20 20 "black")
           ((draw-line ventana2) (make-posn (+ (posicionEnX Balon) 10) (posicionEnY Balon))                         ;; descomentarlo si se ocupa 
                                (make-posn (+ (posicionEnX Balon) 10) (+ (posicionEnY Balon) 20)) "black")
           ((draw-line ventana2) (make-posn (posicionEnX Balon) (+ (posicionEnY Balon) 10))
                                (make-posn (+ (posicionEnX Balon) 20) (+ (posicionEnY Balon) 10)) "black")
           )
         )
        (else
         (begin
           ((draw-solid-ellipse ventana2) (make-posn (posicionEnX Balon) (posicionEnY Balon)) 20 20 "white")
           ((draw-ellipse ventana2) (make-posn (posicionEnX Balon) (posicionEnY Balon)) 20 20 "black")
           ((draw-ellipse ventana2) (make-posn (+ (posicionEnX Balon) (abs (- (fuerzaDelBalon Balon) 20)))
                                              (+ (posicionEnY Balon) (abs (- (fuerzaDelBalon Balon) 20))))
                                   (fuerzaDelBalon Balon) (fuerzaDelBalon Balon) "black")
           ))
        )
  )

(define (movimientoDeJugadores jugadores step nuevosJugadores diagonal angulo Balon)
  (begin
    (cond ((null? jugadores)
           (cond
             ((not (or (< (fuerzaDelBalon Balon) 0) (zero? (fuerzaDelBalon Balon))))
               (begin
                 (dibujarCancha)
                 (Transform nuevosJugadores nuevosJugadores '(210 120))  ;;Nuevo
                 (dibujarBalon (interseccionBalonAux nuevosJugadores Balon))
                 (dibujarVentana)
                 (movimientoDeJugadores '() 0 nuevosJugadores 0 0
                                 (cambiarFuerzaBalon (fuerzaDelBalon (interseccionBalonAux nuevosJugadores Balon))
                                                    ))))
             (else nuevosJugadores))
           )
        ((< diagonal step)
         (movimientoDeJugadores (cdr jugadores) 0
                         (cons (MovimientoFinal (car jugadores)) nuevosJugadores)
                         (DiagonaldelJugador (obtenerSiguienteJugador jugadores))
                         (AnguloJugador (obtenerSiguienteJugador jugadores))
                         (cambiarFuerzaBalon (interseccionBalon (car jugadores)
                                                          (pasosEnX jugadores step diagonal angulo)
                                                          (pasosEnY jugadores step diagonal angulo)
                                                          Balon (append nuevosJugadores (cdr jugadores)))
                                              )))        
        (else
         (begin
           (dibujarCancha)
           ((draw-solid-rectangle ventana2) (make-posn (pasosEnX jugadores step diagonal angulo)
                                                    (pasosEnY jugadores step diagonal angulo))
                                         30 30 "yellow")
           
           (Transform nuevosJugadores nuevosJugadores '(250 120))
           ;(display Balon) (newline) 
           (dibujarBalon (interseccionBalon (car jugadores) (pasosEnX jugadores step diagonal angulo)
                                      (pasosEnY jugadores step diagonal angulo)
                                      Balon (append nuevosJugadores (cdr jugadores))))
           
           (dibujarVentana)
           (movimientoDeJugadores jugadores (+ step 10) nuevosJugadores diagonal angulo
                           (cambiarFuerzaBalon (interseccionBalon (car jugadores) (pasosEnX jugadores step diagonal angulo)
                                                            (pasosEnY jugadores step diagonal angulo) Balon
                                                            (append nuevosJugadores (cdr jugadores)))
                                              ))
           
           ))
        )
    ))





;; Apartir de aqui

;; Genera un tipo de jugador del equipo de la izquierda
(define (Equipo1 posx posy lad )
  (if (equal? lad 'u)
      ((draw-solid-rectangle ventana2) (make-posn posx posy) 30 30 "yellow")
      (if (equal? lad 'd)
          ((draw-solid-rectangle ventana2) (make-posn posx posy) 30 30 "yellow")
          (if (equal? lad 'l)
              ((draw-solid-rectangle ventana2) (make-posn posx posy) 30 30 "yellow")
              (if (equal? lad 'r)
                  ((draw-solid-rectangle ventana2) (make-posn posx posy) 30 30 "yellow")
                  ;else
                  (void)
                  )
              )
      )
 )
  (copy-viewport ventana2 ventana1)
  )
;; Genera un tipo de jugador del equipo de la derecha
(define (Equipo2 posx posy lad )
  (if (equal? lad 'u)
      ((draw-solid-rectangle ventana2) (make-posn posx posy) 30 30 "blue")
      (if (equal? lad 'd)
          ((draw-solid-rectangle ventana2) (make-posn posx posy) 30 30 "blue")
          (if (equal? lad 'l)
              ((draw-solid-rectangle ventana2) (make-posn posx posy) 30 30 "blue")
              (if (equal? lad 'r)
                  ((draw-solid-rectangle ventana2) (make-posn posx posy) 30 30 "blue")
                  ;else
                  (void)
                  )
              )
      )
 )
  (copy-viewport ventana2 ventana1)
  )
(define (Graficar_Equipo Team1 Team2 Bola)
  (cond ((and (null? Team1) (null? Team2))
         void)
        (else
         (begin 
           (Transform (car Team1) (car Team2) Bola)
           (Graficar_Equipo (cdr Team1) (cdr Team2) Bola)))))

(define (Transform AliG1 AliG2 bola)
  (cond ((and (null? AliG1) (null? AliG2))
         void)
        (else
      (begin      
        (Equipo1 (caar (car AliG1)) (cadar (car AliG1))  'r)
        (Equipo2 (caar (car AliG2)) (cadar (car AliG2))  'r)
        
        (Transform (cdr AliG1) (cdr AliG2) bola))
        
      )
     )
  )

;(inicioDelJuego)