;; REQUERIMIENTO 7
;; FUNCION: ASEGURAMIENTO DE LA CALIDAD
;; NATURALEZA: Impura (ejecuta funciones que pueden producir
;;              salida por pantalla)
;; ESTRATEGIA: Validación mediante casos de prueba
;;             representativos de uso normal, alternativo
;;             y erróneo
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
