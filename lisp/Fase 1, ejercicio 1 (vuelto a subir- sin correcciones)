;; FUNCION: valido-datos
;; NATURALEZA: Pura (verifica si los datos pasados por parámetros son válidos)
;; ESTRATEGIA: Orden superior (combina una funcion condicional simple con el llamado a otra funcion si los datos son validos y un mensaje si los datos no son validos)
;; IMPACTO: No destructiva

(Defun valido-datos (color-actual color-siguiente)

    (IF (and (or (equalp color-actual 'en-rojo) (equalp color-actual 'en-amarillo) 
                 (equalp color-actual 'en-verde)) (or (equalp  color-siguiente 'rojo) 
                 (equalp color-siguiente 'amarillo) (equalp color-siguiente 'verde)) 
                 (not (equalp color-actual color-siguiente)) (transicion color-actual color-siguiente) "Error en el ingreso de datos"))

;; FUNCION: transición 
;; NATURALEZA: Pura (muestra el color actual del semaforo y a que color cambia)
;; ESTRATEGIA: Orden superior (muestra el color actual del semaforo y a que color cambia)
;; IMPACTO: No destructiva

(Defun transición (color-actual color-siguiente)
    (List color-actual "cambiar-a" color-siguiente))
