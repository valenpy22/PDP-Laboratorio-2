/*
Nombre: TDA pixhex
Dominio:
Pixhex: Pixhex
- Coordenada X, Coordenada Y: Enteros. 
- HexString: String.
- Profundidad: Entero.

Predicados:
pixhex(X, Y, HEX, D, PIXHEX) (aridad 5)
getx(Pixel, X) (aridad 2)
gety(Pixel, Y) (aridad 2)
gethex(Pixhex, HEX) (aridad 2)
getd(Pixel, Profundidad) (aridad 2)
extraerX(Pixeles, Xs) (aridad 2)
extraerY(Pixeles, Ys) (aridad 2)
hexs(Pixhexs, Colores) (aridad 2)
isPixhex(X, Y, HEX, D) (aridad 4)
cambiarXhex(Pixhex, Largo, PixelFinal) (aridad 3)
cambiarXhexs(Pixhexs, Largo, PixelesFinales) (aridad 3)
cambiarYhex(Pixhex, Largo, PixelFinal) (aridad 3)
cambiarYhexs(Pixhexs, Largo, PixelesFinales) (aridad 3)
pixelAptoHex(Pixhex, X1, Y1, X2, Y2, PixelFinal) (aridad 6)
pixelesAptosHex(Pixhexs, X1, Y1, X2, Y2, PixelesFinales) (aridad 6)
rotateHex(Pixhex, Altura, PixelResultante) (aridad 3)
rotateHexs(Pixhexs, Altura, PixelResultante) (aridad 3)
eliminar_dePixelesHex(Pixhexs, Color, PixhexsFinales) (aridad 3)
ordenarPixelesX(Pixeles, PixelesOrdenadosX) (aridad 2)
ordenarPixelesY(Pixeles, PixelesOrdenadosY) (aridad 2)
filasHex(Pixhexs, String) (aridad 2)
todosHex(Pixhexs, String) (aridad 2)
separarPixelesCada(Numero, Pixeles, Resultado) (aridad 3)
cambiarPixhexs(Pixhexs1, Pixhexs2, Pixhexs3)  (aridad 3)
crearPixelesHexs(Pixhexs, Pixhexs2, Pixhexs3) (aridad 3)

Metas primarias: getx, gety, gethex, getd e isPixhex.
Metas secundarias: extraerX, extraerY, hexs, cambiarXhex, cambiarXhexs,
                   cambiarYhex, cambiarYhexs, pixelAptoHex, pixelesAptosHex,
                   rotateHex, rotateHexs, eliminar_dePixelesHex, ordenarPixelesX,
                   ordenarPixelesY, filasHex, todosHex, separarPixelesCada, 
                   cambiarPixhexs y crearPixelesHexs.
Clausulas
- Reglas:

%Dominio: Cuatro números y pixhex.
%Descripción: Predicado que permite construir el TDA pixhex.
*/
pixhex(X, Y, HEX, D, [X, Y, HEX, D]).

/*
SELECTORES HEX
*/

%Dominio: Un pixhex y un string.
%Descripción: Predicado que permite obtener el valor hexadecimal de un pixhex.
gethex([_, _, Hex|_], Hex).

%Dominio: Pixhexs y lista de colores.
%Descripción: Predicado que permite obtener los colores en hexadecimal de una serie de pixhexs.s
hexs([], []).
hexs([C|Q], N) :-
    gethex(C, HEX),
    hexs(Q, N2),
    my_append([HEX], N2, N).

/*
PERTENENCIA PIXHEX
*/

%Dominio: Dos números, un string y un número.
%Descripción: Predicado que determina si un pixel es de tipo pixhex.
isPixhex(X, Y, HEX, D) :-
    integer(X),
    integer(Y),
    string(HEX),
    integer(D).

/*
MODIFICADORES PIXHEX
*/

%Dominio: Pixhex, número y pixhex.
%Descripción: Predicado que permite actualizar el valor de X de un pixhex para FlipH.
cambiarXhex(Pixel, Largo, Pixelresultante) :-
    getx(Pixel, X),
    gety(Pixel, Y),
    gethex(Pixel, HEX),
    getd(Pixel, D),
    Xnew is Largo-X-1,
    pixhex(Xnew, Y, HEX, D, Pixelresultante).

%Dominio: Pixhexs, número y pixhexs.
%Descripción: Predicado que permite actualizar los valores de X de una serie de pixhexs para FlipH.
cambiarXhexs([], _, []).
cambiarXhexs([Primer|Pixeles], Largo, PixelesFinales) :-
    cambiarXhex(Primer, Largo, Pixel),
    cambiarXhexs(Pixeles, Largo, Pixeles2),
    my_append([Pixel], Pixeles2, PixelesFinales).

%Dominio: Pixhex, número y pixhex.
%Descripción: Predicado que permite actualizar el valor de Y de un pixhex para FlipV.
cambiarYhex(Pixel, Largo, Pixelresultante) :-
    getx(Pixel, X),
    gety(Pixel, Y),
    gethex(Pixel, HEX),
    getd(Pixel, D),
    Ynew is Largo-Y-1,
    pixhex(X, Ynew, HEX, D, Pixelresultante).

%Dominio: Pixhexs, número y pixhexs.
%Descripción: Predicado que permite actualizar los valores de Y de una serie de pixhexs para FlipV.
cambiarYhexs([], _, []).
cambiarYhexs([Primer|Pixeles], Largo, PixelesFinales) :-
    cambiarYhex(Primer, Largo, Pixel),
    cambiarYhexs(Pixeles, Largo, Pixeles2),
    my_append([Pixel], Pixeles2, PixelesFinales).

%Dominio: Pixhex, cuatro números y pixhex o un -1.
%Descripción: Predicado que determina si un pixhex está dentro de un rango dado por los 
%             4 números. Si no lo está, devuelve un -1. Si lo está, devuelve el mismo pixhex.
pixelAptoHex(Pixel, X1, Y1, X2, Y2, P) :-
    getx(Pixel, X),
    gety(Pixel, Y),
    (X < X1;
    X > X2;
    Y < Y1;
    Y > Y2),
    P is -1.
pixelAptoHex(Pixel, X1, Y1, X2, Y2, P) :-
    getx(Pixel, X),
    gety(Pixel, Y),
    gethex(Pixel, HEX),
    getd(Pixel, D),
    X >= X1,
    X =< X2,
    Y >= Y1,
    Y =< Y2,
    pixhex(X, Y, HEX, D, P).

%Dominio: Pixhexs, cuatro números y pixhexs o una lista con -1.
%Descripción: Predicado que obtiene los pixhexs que están dentro de un rango dado por los 
%             4 números. 
pixelesAptosHex([], _, _, _, _, []).
pixelesAptosHex([Primer|Resto], X1, Y1, X2, Y2, PixelesAptos) :-
    pixelAptoHex(Primer, X1, Y1, X2, Y2, P),
    pixelesAptosHex(Resto, X1, Y1, X2, Y2, Pixeles2),
    my_append([P], Pixeles2, PixelesAptos).

%Dominio: Pixhex, número y pixhex.
%Descripción: Predicado que permite rotar un pixhex en 90°.
rotateHex(Pixel, Altura, Pixelresultante) :-
    getx(Pixel, X),
    gety(Pixel, Y),
    gethex(Pixel, HEX),
    getd(Pixel, D),
    Xnew is Altura - 1 - Y,

    pixhex(Xnew, X, HEX, D, Pixelresultante).

%Dominio: Pixhexs, número y pixhexs.
%Descripción: Predicado que permite rotar una serie de pixhexs en 90°.
rotateHexs([], _, []).
rotateHexs([Primer|Pixeles], Altura, PixelesFinales) :-
    rotateHex(Primer, Altura, Pixel),
    rotateHexs(Pixeles, Altura, Pixeles2),
    my_append([Pixel], Pixeles2, PixelesFinales).

%Dominio: Pixhexs, string y pixhexs.
%Descripción: Predicado que elimina los pixhex que tengan el mismo color que el color dado.
eliminar_dePixelesHex([], _, []).
eliminar_dePixelesHex([Primer|_], E, _) :-
    gethex(Primer, HEX),
    HEX = E,
    !.
eliminar_dePixelesHex([Primer|Pixeles], E, L) :-
    eliminar_dePixelesHex(Pixeles, E, N),
    my_append([Primer], N, L).

%Dominio: Pixhexs y string.
%Descripción: Predicado que permite transformar una fila de pixhexs a string.
filasHex([], "\n").
filasHex([Pixel|Resto], String) :-
    gethex(Pixel, HEX),
    string_concat(HEX, "\t", String1),
    filasHex(Resto, String2),
    string_concat(String1, String2, String).

%Dominio: Pixhexs y string.
%Descripción: Predicado que permite transformar una matriz de pixhexs a string.
todosHex([], "\n").
todosHex([Pixel|Resto], String) :-
    filasHex(Pixel, String1),
    todosHex(Resto, String2),
    string_concat(String1, String2, String).

%Dominio: Pixhexs, pixhexs y pixhexs.
%Descripción: Predicado que permite cambiar el color de un pixhex si su profundidad
%             es distinta al de otro pixhex.
cambiarPixhexs([_|_], [], []).
cambiarPixhexs([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales) :-
    getx(Pixel2, X),
    gety(Pixel2, Y),
    getd(Pixel1, D1),
    getd(Pixel2, D2),
    D1 \== D2,
    pixhex(X, Y, "#FFFFFF", D1, P1),
    cambiarPixhexs([Pixel1|Pixeles1], Pixeles2, PixelesFinales2),
    my_append([P1], PixelesFinales2, PixelesFinales).
cambiarPixhexs([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales) :-
    getx(Pixel2, X),
    gety(Pixel2, Y),
    gethex(Pixel2, HEX),
    getd(Pixel1, D1),
    getd(Pixel2, D2),
    D1 = D2,
    pixhex(X, Y, HEX, D1, P1),
    cambiarPixhexs([Pixel1|Pixeles1], Pixeles2, PixelesFinales2),
    my_append([P1], PixelesFinales2, PixelesFinales).

%Dominio: Pixhexs, pixhexs y pixhexs.
%Descripción: Predicado que permite crear listas de pixhexs agrupados según su profundidad.
crearPixelesHexs([], [_|_], []).
crearPixelesHexs([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales) :-
    cambiarPixhexs([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales1),
    crearPixelesHexs(Pixeles1, [Pixel2|Pixeles2], PixelesFinales2),
    my_append([PixelesFinales1], PixelesFinales2, PixelesFinales3),
    eliminar_duplicados(PixelesFinales3, PixelesFinales).