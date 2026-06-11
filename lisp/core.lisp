
;; REQUERIMIENTO 6
;; FUNCION: distribucion-temporal
;; NATURALEZA: Impura/efectos secundarios (imprime información en pantalla mediante FORMAT)
;; ESTRATEGIA: Función de orden superior (utiliza MAPCAR para calcular los porcentajes de cada color)
;; IMPACTO: No destructiva

(defun distribucion-temporal ()

  (let ((porcentajes
          (mapcar
           #'(lambda (segundos)
               (* (/ segundos 216.0) 100))
           '(90 6 120))))

    (format t
            "Rojo: ~,2F%~%Amarillo: ~,2F%~%Verde: ~,2F%~%"
            (first porcentajes)
            (second porcentajes)
            (third porcentajes))))