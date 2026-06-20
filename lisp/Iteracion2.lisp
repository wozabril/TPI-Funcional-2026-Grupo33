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
    ((and (equalp color_actual 'en-rojo) (equalp color_siguiente 'amarillo))
     (list 'en-rojo "cambiar-a-rojo-intermitente"))
    
    ((and (equalp color_actual 'en-amarillo) (equalp color_siguiente 'verde))
     (list 'en-amarillo "cambiar-a-amarillo-intermitente"))
    
    ((and (equalp color_actual 'en-verde) (equalp color_siguiente 'rojo))
     (list 'en-verde "cambiar-a-verde-intermitente"))
     
    ;; Transiciones para salir de la intermitencia al color definitivo
    ((and (equalp color_actual 'en_rojo_intermitente) (equalp color_siguiente 'amarillo))
     (list 'en-rojo "cambiar-a-amarillo"))
     
    ((and (equalp color_actual 'en_amarillo_intermitente) (equalp color_siguiente 'verde))
     (list 'en-amarillo "cambiar-a-verde"))
     
    ((and (equalp color_actual 'en_verde_intermitente) (equalp color_siguiente 'rojo))
     (list 'en-verde "cambiar-a-rojo"))
    
    ;; Caso por defecto si no entra en nada de lo de arriba
    (t 
     (list color_actual 'accion-por-defecto))
  )
)

;; FUNCION: timer-intermitente
;; NATURALEZA: Pura (para un mismo timestamp siempre devuelve
;;              el mismo estado del semáforo, sin efectos secundarios)
;; ESTRATEGIA: Construcción directa mediante operaciones aritméticas
;;             y estructura condicional múltiple (COND). Utiliza MOD
;;             para determinar la posición temporal dentro del ciclo
;;             completo y asociarla al estado correspondiente.
;; IMPACTO: No destructiva (no modifica datos ni estructuras)

(defun timer-intermitente (timestamp)

  (cond
    ((< (mod timestamp 225) 90)
     'en-rojo)

    ((< (mod timestamp 225) 93)
     'en_rojo_intermitente)

    ((< (mod timestamp 225) 213)
     'en-amarillo)

    ((< (mod timestamp 225) 216)
     'en_amarillo_intermitente)
     
    ((< (mod timestamp 225) 222)
     'en-verde)

    ((< (mod timestamp 225) 225)
     'en_verde_intermitente)))




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
