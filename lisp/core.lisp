;; REQUERIMIENTO 1: ESTADOS DE TRANSICION
;; FUNCION: valido-datos
;; NATURALEZA: Pura (verifica si los datos pasados por parámetros son válidos)
;; ESTRATEGIA: Orden superior (combina una funcion condicional simple con el llamado a otra funcion si los datos son validos y un mensaje si los datos no son validos)
;; IMPACTO: No destructiva

(defun valido-datos (color-actual color-siguiente)

  (if (and
       (or (equalp color-actual 'en-rojo)
           (equalp color-actual 'en-amarillo)
           (equalp color-actual 'en-verde))

       (or (equalp color-siguiente 'rojo)
           (equalp color-siguiente 'amarillo)
           (equalp color-siguiente 'verde))

       (not (or
             (and (equalp color-actual 'en-rojo)
                  (equalp color-siguiente 'rojo))

             (and (equalp color-actual 'en-amarillo)
                  (equalp color-siguiente 'amarillo))

             (and (equalp color-actual 'en-verde)
                  (equalp color-siguiente 'verde))))
      )
      
      (transicion color-actual color-siguiente)

      "Error en el ingreso de datos"))


;; FUNCION: transición 
;; NATURALEZA: Pura (muestra el color actual del semaforo y a que color cambia)
;; ESTRATEGIA: Orden superior (muestra el color actual del semaforo y a que color cambia)
;; IMPACTO: No destructiva


(defun transicion (color_actual color_siguiente)

  (cond

    ((and (equalp color_actual 'en-rojo)
          (equalp color_siguiente 'amarillo))
     (list color_actual "cambiar-a-amarillo"))

    ((and (equalp color_actual 'en-amarillo)
          (equalp color_siguiente 'verde))
     (list color_actual "cambiar-a-verde"))

    ((and (equalp color_actual 'en-verde)
          (equalp color_siguiente 'rojo))
     (list color_actual "cambiar-a-rojo"))

    (t
     (list color_actual 'accion-por-defecto))))


;;REQUERIMENTO 2: FUNCION TIMER
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
    ((< (mod timestamp 216) 90) 'en-rojo)
    ((< (mod timestamp 216) 96) 'en-amarillo)
    ((< (mod timestamp 216) 216) 'en-verde)))



;;REQUERIMIENTO 3: SISTEMA DE AUDITORIA
;; FUNCIÓN: auditar-cambio
;; NATURALEZA: Impura (imprime información en pantalla mediante format)
;; ESTRATEGIA: Función de composición (utiliza timer para obtener
;;             el estado anterior y actual y luego los compara)
;; IMPACTO: No destructiva (no modifica estructuras ni variables)

(defun cambio-estado-luz (timestamp)
  (let ((anterior (timer (- timestamp 1)))
        (actual   (timer timestamp)))
    (if (not (eq anterior actual))
        (format t
                "Tiempo ~A: la luz ha cambiado de ~A a ~A~%"
                timestamp
                anterior
                actual) 
        (format t
                “tiempo ~A: no hubo cambio de estado ~%” 
                  Timestamp))))




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



;;REQUERIMIENTO 5: PLANIFICACION TEMPORAL
;; FUNCIÓN: ciclos-por-tiempo
;; NATURALEZA: Pura (Dado un número de minutos, siempre
;;             retorna la misma cantidad de ciclos)
;; ESTRATEGIA: Composición (utiliza duracion-ciclo, floor
;;             y nth-value para calcular los ciclos completos)
;; IMPACTO: No destructiva

(defun ciclos-por-tiempo (minutos)
   (if (and (numberp minutos)
            (>= minutos 0))
       (nth-value 0
         (floor(* minutos 60)216))
       "ERROR EN EL INGRESO DE DATOS"))


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

;; REQUERIMIENTO 7
;; FUNCION: ASEGURAMIENTO DE LA CALIDAD
;; NATURALEZA: Impura (ejecuta funciones que pueden producir salida por pantalla)
;; ESTRATEGIA: Validación mediante casos de prueba representativos de uso normal, alternativo y erróneo
;; IMPACTO: No destructiva

;; REQ 1
(valido-datos 'en-rojo 'amarillo)
(valido-datos 'en-verde 'amarillo)
(valido-datos 'azul 'verde)

;; REQ 2
(siesentero 100)
(siesentero "hola")

;; REQ 3
(cambio-estado-luz 90)
(cambio-estado-luz 50)

;; REQ 4
(ingreso-ciclo 216)
(ingreso-ciclo "abc")

;; REQ 5
(ciclos-por-tiempo 15)
(ciclos-por-tiempo -5)

;; REQ 6

(distribucion-temporal)

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

;; FUNCION: timer 
;; NATURALEZA: pura  devuelve un valor para el tipo de color segun el tiempo dado
;; ESTRATEGIA: mediante función aritmética y un condicional multiple se determina a que color se hacer referencia según el tiempo
;; IMPACTO: no destructiva



(defun timer-intermitente (timestamp)

  (cond
    ((< (mod timestamp 222) 90) 'en-rojo)
    ((< (mod timestamp 222) 93) 'amarillo-intermitente)
    ((< (mod timestamp 222) 213) 'en-verde)
    ((< (mod timestamp 222) 216) 'amarillo-intermitente)
    ((< (mod timestamp 222) 222) 'en-rojo)))


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
