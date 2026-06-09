;; REQUERIMIENTO 1: ESTADOS DE TRANSICION
;; FUNCION: valido-datos
;; NATURALEZA: Pura (verifica si los datos pasados por parámetros son válidos)
;; ESTRATEGIA: Orden superior (combina una funcion condicional simple con el llamado a otra funcion si los datos son validos y un mensaje si los datos no son validos)
;; IMPACTO: No destructiva

(Defun valido-datos (color-actual color-siguiente)
     (IF (and (or (equalp color-actual 'en-rojo)
                  (equalp color-actual 'en-amarillo) 
                  (equalp color-actual 'en-verde)) 
                  
              (or (equalp color-siguiente 'rojo) 
              (equalp color-siguiente 'amarillo) 
              (equalp color-siguiente 'verde))

              (and (not (and (equalp color-actual 'en-rojo) (equalp color-siguiente 'rojo)) 
              (not (and (equalp color-actual 'en-amarillo) (equalp color-siguiente 'amarillo)) 
              (not (and (equalp color-actual 'en-verde) (equalp color-siguiente 'verde)))))
              
(transicion color-actual color-siguiente) "Error en el ingreso de datos"))))

 

;; FUNCION: transición 
;; NATURALEZA: Pura (muestra el color actual del semaforo y a que color cambia)
;; ESTRATEGIA: Orden superior (muestra el color actual del semaforo y a que color cambia)
;; IMPACTO: No destructiva

(defun transicion (color_actual color_siguiente)
    (COND 
        ((AND (EQUALP COLOR_ACTUAL 'en_rojo) (equalp color_siguiente 'amarillo)) (format t "~a pasar-a~a" color_actual color_siguiente))
        ((AND (EQUALP COLOR_ACTUAL 'en_amarillo) (equalp color_siguiente 'verde)) (format t "~a pasar-a~a" color_actual color_siguiente))
        ((AND (EQUALP COLOR_ACTUAL 'en_verde) (equalp color_siguiente 'amarillo)) (format t "~a pasar-a~a" color_actual color_siguiente))
        ((AND (EQUALP COLOR_ACTUAL 'en_amarillo) (equalp color_siguiente 'ro)) (format t "~a pasar-a~a" color_actual color_siguiente))   
    )
)

;; REQUERIMIENTO 4: ANALISIS DE CICLOS SEMAFORICOS
;; FUNCION: ingreso-ciclo
;; NATURALEZA: Pura (Efectua la validacion de datos que recibe como parámetros)
;; ESTRATEGIA: Orden superior (Verifica que se reciba una cantidad de segundos y luego llama a las funciones duracion-ciclo y evaluacion-rango)
;; IMPACTO: No destructiva

(DEFUN ingreso-ciclo (segundos)
    (if (numberp segundos) (progn
        (print (duracion-ciclo segundos))
        (recomendacion-ciclo segundos)) "ERROR EN EL INGRESO DE DATOS"))

;; FUNCION: duracion-ciclo
;; NATURALEZA: Pura (Dada una cantidad de segundos, retorna la cantidad de ciclos que se efectuan en ese tiempo)
;; ESTRATEGIA: Orden superior ()
;; IMPACTO: No destructiva

(defun duracion-ciclo (segundos)
    (/ segundos 216))

;; FUNCION: recomendacion-ciclo
;; NATURALEZA: Pura (Dada una cantidad de segundos, retorna una breve evaluacion del ciclo)
;; ESTRATEGIA: Estructura condicional (Evalua si la duracion del ciclo esta dentro del rango optimo)
;; IMPACTO: No destructiva

(defun recomendacion-ciclo (segundos)
    (cond
        ((< segundos 35) "Ciclo demasiado corto")
        ((> segundos 150) "Ciclo demasiado largo")
        (T "Ciclo optimo")))
