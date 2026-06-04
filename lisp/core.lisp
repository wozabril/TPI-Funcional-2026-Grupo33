;;FUNCION: siesentero
;;NATURALEZA: con efectos secundarios (tiene salida por pantalla si falla)
;;ESTRATEGIA: implementa un condicional para verificar que el valor de timestamp sea valido para luego implemetar la función timer
;;IMPACTO: no destructiva

(defun siesentero (timestamp)

 (if (integerp timestamp)
 
  (timer timestamp)
  (format t "no es una hora valida")
  
 )

)



;; FUNCION: timer 
;; NATURALEZA: pura  devuelve un valor para el tipo de color segun el tiempo dado
;; ESTRATEGIA: mediante función aritmética y un condicional multiple se determina a que color se hacer referencia según el tiempo
;; IMPACTO: no destructiva


(defun timer (timestamp)
   (cond
    ((< (mod timestamp 216) 91) 'en-rojo )
    ((< (mod timestamp 216) 97)  'en-amarillo)
    ((< (mod timestamp 216) 216)  'en-verde )
   )
  )