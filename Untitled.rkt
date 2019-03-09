
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
(define ventana1 (open-viewport "*ConcaChampionsCE*" 800 700))
(define ventana2 (open-pixmap "*ConcaChampionsCE*" 800 700))
(define jugadores '(((340 150) (455 260) 20 1) ((300 100) (700 400) 16 2) ((700 400) (300 300) 10 3)  )) ;; LISTA DE JUGADORES
(define balon '((400 200) 0 45))


;;Mostrando el Marcador y generaciones
(define (mostrarDatos marcador generacion)
  (begin
    ((draw-string ventana2) (make-posn 430 600) (number->string (car marcador)) "white")
    ((draw-string ventana2) (make-posn 490 600) (number->string (cadr marcador)) "white")
    ((draw-string ventana2) (make-posn 600 650) "Generacion: " "black")
    ;;ACÁ ABAJO SE LE PASA EL PARAMETRO DE LA GENERACION EN LA QUE QUEDÓ
    ))

;;Dibujando la Cancha
(define (dibujarCancha)
  (begin
    ((draw-solid-rectangle ventana2) (make-posn 0 0) 800 700 "Dark Gray") ;;Background
    ((draw-solid-rectangle ventana2) (make-posn 0 0) 800 540 "Forest Green") ;;Pasto
    ((draw-solid-rectangle ventana2) (make-posn 0 0) 5 540 "black")   ;;Linea vertical
    ((draw-solid-rectangle ventana2) (make-posn 795 0) 5 540 "black") ;;Linea vertical
    ((draw-solid-rectangle ventana2) (make-posn 0 0) 920 5 "black")   ;;Linea horizontal
    ((draw-solid-rectangle ventana2) (make-posn 0 535) 920 5 "black") ;;Linea horizontal
    ((draw-solid-rectangle ventana2) (make-posn 405 0) 5 540 "black") ;;Linea medio campo
    ((draw-solid-ellipse ventana2) (make-posn 400 262) 15 15 "black") ;;Circulo de posicion de la Balon
    ((flip-ellipse ventana2) (make-posn 333 195) 150 150 "black")     ;;Circulo central
    ((flip-rectangle ventana2) (make-posn -130 180) 180 180 "black")   ;;Area equipo izquierdo
    ((flip-rectangle ventana2) (make-posn -100 210) 120 120 "white")   ;;Marco equipo izquierdo
    ((flip-rectangle ventana2) (make-posn 750 180) 180 180 "black")   ;;Area equipo derecho
    ((flip-rectangle ventana2) (make-posn 780 210) 120 120 "white")   ;;Marco equipo izquierdo
    ))

(define (dibujarVentana)
  (begin
    (copy-viewport ventana2 ventana1)
    ((clear-viewport ventana2))
    )
  )

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
        ((and (> (posicionEnX Balon) 889) (or (< (posicionEnY Balon) 165) (> (posicionEnY Balon) 360)))
         (list (list 889 (posicionEnY Balon)) (fuerzaDelBalon Balon) (+ (anguloDelBalon Balon) 240)))
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
           ((draw-line ventana2) (make-posn (+ (posicionEnX Balon) 10) (posicionEnY Balon))
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


(inicioDelJuego)

