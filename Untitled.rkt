
#lang racket
(require graphics/graphics)
(require 2htdp/image)
(require racket/math)

(open-graphics)


;;-----------------------------------------------------------------------------------------------------------------------------
;;                                               INTERFAZ
;;-----------------------------------------------------------------------------------------------------------------------------
;;INICIO DEL JUEGO

(define (inicioDelJuego)
  (begin
    (dibujarCancha)
    (mostrarDatos '(1 0) 12)
    
    (movimientoDeJugadores jugadores 0 '() (DiagonaldelJugador (car jugadores)) (AnguloJugador (car jugadores)) balon)
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
(define jugadoresM '( ((450 250) (700 400) 16 2) ((458 300) (700 400) 16 2) ((70 400) (300 300) 10 3) ((70 100) (455 260) 20 1) ((10 250) (700 400) 16 2) ((70 200) (700 400) 16 2) ((200 70) (700 400) 16 2) ((210 150) (700 400) 16 2)  ((230 250) (700 400) 16 2) ((210 350) (700 400) 16 2) ((200 440) (700 400) 16 2))) ;; LISTA DE JUGADORES M

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
                 (dibujarJugadores  nuevosJugadores 1)
                 (dibujarBalon (interseccionBalonAux nuevosJugadores Balon))
                 (dibujarVentana)
                 (movimientoDeJugadores '() 0 nuevosJugadores 0 0
                                 (cambiarFuerzaBalon (fuerzaDelBalon (interseccionBalonAux nuevosJugadores Balon))
                                                    ))))
             (else nuevosJugadores)) )
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
           
           (dibujarJugadores (append nuevosJugadores (cdr jugadores)) 1)
           (display Balon) (newline) 
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










;;Hace una lista con las dos listas en una sola, de  jugadores.
(define (HacerUnalista equipos)
  (append (car equipos) (cadr equipos))
  )



;;(define JugadoresInciales (movimiento-aux (ListadeequipoIzquierda '(4 4 2)) (ListadeequipoDerecha '(4 3 3)) balon))



;;Acá inicia la primera generacion para ambos equipos


;;BORRARLA SI NO LA OCUPAN PORQUE LA HICE SIN QUERER xD 

(define (jugador-1er tipo num)
  (cond((equal? 1 tipo)
        (list (list 5 (+ 180 (random 180))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 1 num "yellow"))
       
       ((equal? 2 tipo)
        
        (list (list (+ 90 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 2 num "yellow"))
       
       ((equal? 3 tipo)
        
        (list (list (+ 260 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 3 num "yellow"))
       
       (else
        
        (list (list (+ 400 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 4 num "yellow"))
       ))

(define (jugador-2do tipo num)
  (cond ((equal? 1 tipo)
         
         (list (list 895 (+ 180 (random 180))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 1 num "black"))
        
        ((equal? 2 tipo)
         
         (list (list (+ 750 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 2 num "black"))
        
        ((equal? 3 tipo)
         
         (list (list (+ 600 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 3 num "black"))
        
        (else
         
         (list (list (+ 465 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 4 num "black"))
        ))

;;Estas funciones definen las listas de quipos
(define (ListadeequipoIzquierda formacion)
  
  (cond ((not(list? formacion)) '())
        (else
         
         (StartListadeequipoIzquierda (+ 1 (car formacion) (cadr formacion) (caddr formacion)) formacion))
        ))

(define (ListadeequipoDerecha formacion)
  
  (cond ((not(list? formacion)) '())
        (else
         
         (StartListadeequipoDerecha (+ 1 (car formacion) (cadr formacion) (caddr formacion)) formacion))
        ))

;;Inicializa al equipo con su formacion
(define (StartListadeequipoIzquierda numero formacion)
  (cond((zero? numero) '())
       ((equal? 1 numero)
        (cons (jugador-1er 1 1) (StartListadeequipoIzquierda (- numero 1) formacion)))
       ((and (> numero 1) (< numero (+ 2 (car formacion))))
        (cons (jugador-1er 2 numero) (StartListadeequipoIzquierda (- numero 1) formacion)))
       ((and (> numero (+ 1 (car formacion))) (< numero (+ 2 (car formacion) (cadr formacion))))
        (cons (jugador-1er 3 numero) (StartListadeequipoIzquierda (- numero 1) formacion)))
       (else
        (cons (jugador-1er 4 numero) (StartListadeequipoIzquierda (- numero 1) formacion)))
       ))

(define (StartListadeequipoDerecha numero formacion)
  (cond ((zero? numero) '())
        
        ((equal? 1 numero)
         
         (cons (jugador-2do 1 1) (StartListadeequipoDerecha (- numero 1) formacion)))
        
        ((and (> numero 1) (< numero (+ 2 (car formacion))))
         
         (cons (jugador-2do 2 numero) (StartListadeequipoDerecha (- numero 1) formacion)))
        
        ((and (> numero (+ 1 (car formacion))) (< numero (+ 2 (car formacion) (cadr formacion))))
         
         (cons (jugador-2do 3 numero) (StartListadeequipoDerecha (- numero 1) formacion)))
        
        (else
         
         (cons (jugador-2do 4 numero) (StartListadeequipoDerecha (- numero 1) formacion)))
        ))



;;Con esta función se parte la lista de jugadores a la mitad
(define (partirListaDeJugadoresAux jugadores num nuevos)
  
  (cond ((> num 10) (list nuevos jugadores))
        
        (else (partirListaDeJugadoresAux (cdr jugadores) (+ num 1) (cons (car jugadores) nuevos)) ))
  )

(define (partirListaDeJugadores jugadores)
  
  (begin
    
    (display jugadores) (newline)
    
    (partirListaDeJugadoresAux jugadores 0 '() )
    ))

                                                              ;;FUNCIONES QUE FALTAN POR PROBARSE
;Funcion principal para mover los 2 equipos, llama a su cada respectiva funcion de movimiento
;(define (movimiento listaGrande)
  ;;(movimiento-aux (cadr (mitaddeEquipos (car listaGrande))) (car (mitaddeEquipos (car listaGrande))) (cadr listaGrande))
;;  )


;;(define (movimiento-aux jugadores jugadores_S ball)
 ;; (cond((or (null? jugadores) (null? jugadores)) '())
      ;; (else
        ;;(list (movimiento_jugadores jugadores '() ball) (movimiento_jugadores_S jugadores_S '() ball)))))




;Funcion que reliaza el movimento del equipo del lado izquierda
;;(define (movimiento_jugadores jugadores listaF ball)
  ;;(cond ((null? jugadores) listaF)
        ;;(else
         ;;(movimiento_jugadores (cdr jugadores) (append listaF (list (movimiento_a_bola (car jugadores) listaF ball))) ball))))




;;Acá se obtienen los datos detallados de un solo jugador 
;;Obtiene las posiciones iniciales X y Y
(define (obtenerposicioninicialXY jugador)
  
  (cond((or (null? jugador) (number? jugador)) '(50 50))

       (else
        
        (car jugador))))

;;Obtiene las posiciones finales X y Y
(define (obtenerposicionfinalXY jugador)
  
  (cond((null? jugador) '())

       (else
        
        (cadr jugador))))





;;(define (Partido formacion1 formacion2 iteraciones)
  ;;(cond ((or (null? formacion1) (null? formacion2)) '())
       ;; (else
        ;; (begin
         ;;  (XXX (HacerUnalista JugadoresInciales) 1 firstBall '(0 0)) ;;En XXX se pone el nombre de la funcion que maneja la generacion y el marcador
        ;;   ))
 ;; ))

;;(Partido '(4 4 2) '(4 3 3) 20);; Acá se establecen las formaciones
(inicioDelJuego)