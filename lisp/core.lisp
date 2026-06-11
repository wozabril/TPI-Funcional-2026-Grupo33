
;; REQUERIMIENTO 6
;; FUNCION: distribucion-temporal
;; NATURALEZA: Impura (imprime información en pantalla mediante FORMAT)
;; ESTRATEGIA: Construcción directa (calcula los porcentajes mediante fórmulas aritméticas sin recursión ni funciones de orden superior)
;; IMPACTO: No destructiva (no modifica estructuras ni variables)

(defun distribucion-temporal ()

  (format t
          "Rojo: ~,2F%~%Amarillo: ~,2F%~%Verde: ~,2F%~%"
          (* (/ 90.0 216) 100)
          (* (/ 6.0 216) 100)
          (* (/ 120.0 216) 100)))