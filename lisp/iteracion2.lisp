;; =================================================================
;; ITERACION 2 - EXTENSION 1: Intermitencia
;; =================================================================

;; FUNCION: transicion 
;; NATURALEZA: Pura (depende solo de los parametros y retorna una lista, sin side-effects)
;; ESTRATEGIA: Estructura condicional (usamos cond para evaluar los casos)
;; IMPACTO: No destructiva

(defun transicion (color_actual color_siguiente)
  (cond
    ;; Transiciones para agregar los 3 seg de intermitencia
    ((and (equalp color_actual 'en_rojo) (equalp color_siguiente 'verde))
     (list 'en_rojo_intermitente "cambiar-a-rojo-intermitente"))
    
    ((and (equalp color_actual 'en_verde) (equalp color_siguiente 'amarillo))
     (list 'en_verde_intermitente "cambiar-a-verde-intermitente"))
    
    ((and (equalp color_actual 'en_amarillo) (equalp color_siguiente 'rojo))
     (list 'en_amarillo_intermitente "cambiar-a-amarillo-intermitente"))
     
    ;; Transiciones para salir de la intermitencia al color definitivo
    ((and (equalp color_actual 'en_rojo_intermitente) (equalp color_siguiente 'verde))
     (list 'en_verde "cambiar-a-verde"))
     
    ((and (equalp color_actual 'en_verde_intermitente) (equalp color_siguiente 'amarillo))
     (list 'en_amarillo "cambiar-a-amarillo"))
     
    ((and (equalp color_actual 'en_amarillo_intermitente) (equalp color_siguiente 'rojo))
     (list 'en_rojo "cambiar-a-rojo"))
    
    ;; Caso por defecto si no entra en nada de lo de arriba
    (t 
     (list color_actual 'accion-por-defecto))
  )
)


;; =================================================================
;; ITERACION 2 - EXTENSION 2: Guardar en archivo
;; =================================================================

;; FUNCION: escribir-log-recursivo
;; NATURALEZA: Impura (porque escribe datos en el stream del archivo)
;; ESTRATEGIA: Recursiva de cola (la armamos para iterar la lista sin usar loop)
;; IMPACTO: No destructiva (no modifica la lista original de datos)

(defun escribir-log-recursivo (stream datos)
  (cond
    ;; Caso base: llegamos al final de la lista, cortamos la recursion
    ((endp datos) t)
    
    ;; Caso recursivo: escribo la linea actual y vuelvo a llamar con el resto (cdr)
    (t 
     (format stream "~a~%" (car datos))
     (escribir-log-recursivo stream (cdr datos)))
  )
)

;; FUNCION: informe
;; NATURALEZA: Impura (crea y abre un archivo de texto en la pc)
;; ESTRATEGIA: Llamada a funcion (usa nuestra funcion recursiva auxiliar)
;; IMPACTO: No destructiva

(defun informe (datos)
  ;; Abrimos el archivo, y le decimos que si ya existe, lo pise con lo nuevo
  (with-open-file (stream "informe-ejecucion-semaforo.txt" 
                          :direction :output 
                          :if-exists :supersede
                          :if-does-not-exist :create)
    
    (format stream "Informe de Ejecución del Sistema Semafórico~%")
    (format stream "=============================================~%")
    
   ;; Invocamos a la funcion recursiva encargada de iterar la lista y generar el log.
    (escribir-log-recursivo stream datos)
    
    (format stream "=============================================~%")
    (format stream "~% --- Fin del Informe ---")
  )
)
