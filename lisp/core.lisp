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
