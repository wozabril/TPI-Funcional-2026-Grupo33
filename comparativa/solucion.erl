-module(solucion).
-export([timer/1, transicion/2]).

%% ========================================================
%% FUNCIÓN: timer
%% NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
%% ESTRATEGIA: Función Predicado (Lógica condicional basada en módulo 216)
%% IMPACTO: No destructiva
%% ========================================================
timer(Timestamp) ->
    Modulo = Timestamp rem 216,
    if
        Modulo < 90 -> en_rojo;
        Modulo < 96 -> en_amarillo;
        true -> en_verde
    end.

%% ========================================================
%% FUNCIÓN: transicion
%% NATURALEZA: Pura (Retorna una lista con el estado y la acción)
%% ESTRATEGIA: Pattern Matching (Cláusulas de guarda para estados)
%% IMPACTO: No destructiva
%% ========================================================
transicion(Color_actual, Color_siguiente) ->
    case {Color_actual, Color_siguiente} of
        {en_rojo, amarillo} -> [en_rojo, "cambiar-a-amarillo"];
        {en_amarillo, verde} -> [en_amarillo, "cambiar-a-verde"];
        {en_verde, amarillo} -> [en_verde, "cambiar-a-amarillo"];
        {en_amarillo, rojo} -> [en_amarillo, "cambiar-a-rojo"];
        _ -> [Color_actual, "accion-por-defecto"]
    end.

