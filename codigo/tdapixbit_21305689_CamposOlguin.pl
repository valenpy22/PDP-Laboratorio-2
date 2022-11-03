/*
Nombre: TDA pixbit
Dominio:
Pixbit: Pixbit
- Coordenada X, Coordenada Y, Bit, Profundidad: Enteros.

Predicados:
pixbit(X, Y, BIT, D, PIXBIT) (aridad 5)
getx(Pixel, X) (aridad 2)
gety(Pixel, Y) (aridad 2)
getbit(Pixbit, BIT) (aridad 2)
getd(Pixel, Profundidad) (aridad 2)
bits(Pixbits, BITS) (aridad 2)
extraerX(Pixeles, Xs) (aridad 2)
extraerY(Pixeles, Ys) (aridad 2)
isPixbit(X, Y, BIT, Depth)  (aridad 4)
cambiarXbit(Pixbit, Largo, PixelFinal) (aridad 3)
cambiarXbits(Pixbits, Largo, PixelesFinales) (aridad 3)
cambiarYbit(Pixbit, Largo, PixelFinal) (aridad 3)
cambiarYbits(Pixbits, Largo, PixelesFinales) (aridad 3)
pixelAptoBit(Pixbit, X1, Y1, X2, Y2, PixelFinal) (aridad 6)
pixelesAptosBit(Pixbits, X1, Y1, X2, Y2, PixelesFinales) (aridad 6)
rotateBit(Pixbit, Altura, PixelFinal) (aridad 3)
rotateBits(Pixbits, Altura, PixelesFinales) (aridad 3)
eliminar_dePixelesBit(Pixbits, Color, PixelesFinales) (aridad 3)
ordenarPixelesX(Pixeles, PixelesOrdenadosX) (aridad 2)
ordenarPixelesY(Pixeles, PixelesOrdenadosY) (aridad 2)
filasBit(Pixbits, String) (aridad 2)
todosBit(Pixbits, String) (aridad 2)
separarPixelesCada(Numero, Pixeles, Resultado) (aridad 3)
cambiarPixbits(Pixbits, Pixbits2, PixbitsFinales) (aridad 3)
crearPixelesBits(Pixbits, Pixbits2, PixbitsFinales) (aridad 3)

Metas primarias: getx, gety, getbit, getd e isPixbit.
Metas secundarias: bits, extraerX, extraerY, cambiarXbit, cambiarXbits, cambiarYbit, cambiarYbits,
                   pixelAptoBit, pixelesAptosBit, rotateBit, rotateBits, eliminar_dePixelesBit, ordenarPixelesX
                   ordenarPixelesY, filasBit, todosBit, separarPixelesCada, cambiarPixbits y crearPixelesBits.

Representación: integer X integer X integer X integer X list

Clausulas
- Reglas:

%Dominio: Cuatro números y pixbit.
%Descripción: Predicado que permite construir el TDA pixbit.
*/
pixbit(X, Y, BIT, D, [X, Y, BIT, D]).

/*
SELECTORES BITS
*/
%Dominio: Un pixel y un número.
%Descripción: Predicado que permite obtener el valor de X de un pixel.
getx([X|_], X).

%Dominio: Un pixel y un número.
%Descripción: Predicado que permite obtener el valor de Y de un pixel.
gety([_, Y|_], Y).

%Dominio: Un pixbit y un número.
%Descripción: Predicado que permite obtener el valor de bit de un pixbit.
getbit([_, _, BIT|_], BIT).

%Dominio: Un pixbit o pixhex, y un número.
%Descripción: Predicado que permite obtener el valor de la profundidad del pixel.
getd([_, _, _, D|_], D).

%Dominio: Pixbits y lista con los valores de bit de cada pixbit.
%Descripción: Predicado que permite obtener los valores de los bits de cada pixbit.
bits([], []).
bits([C|Q], N) :-
    getbit(C, BIT),
    bits(Q, N2),
    my_append([BIT], N2, N).

%Dominio: Pixeles y lista con los valores de X de cada pixel.
%Descripción: Predicado que permite obtener los valores de X de cada pixel.
extraerX([], []).
extraerX([Pixel|Pixeles], L) :-
    getx(Pixel, X),
    extraerX(Pixeles, N),
    my_append([X], N, L).

%Dominio: Pixeles y lista con los valores de Y de cada pixel.
%Descripción: Predicado que permite obtener los valores de Y de cada pixel.
extraerY([], []).
extraerY([Pixel|Pixeles], L) :-
    gety(Pixel, Y),
    extraerY(Pixeles, N),
    my_append([Y], N, L).

/*
PERTENENCIA PIXBIT
*/

%Dominio: Cuatro números.
%Descripción: Predicado que determina si un pixel es de tipo pixbit.
isPixbit(X, Y, BIT, D) :-
    integer(X),
    integer(Y),
    integer(BIT),
    integer(D),
    (BIT is 0;
    BIT is 1).

/*
MODIFICADORES PIXBIT
*/

%Dominio: Pixbit, número y pixbit.
%Descripción: Predicado que permite cambiar el valor de X de un pixbit para FlipH.
cambiarXbit(Pixel, Largo, Pixelresultante) :-
    getx(Pixel, X),
    gety(Pixel, Y),
    getbit(Pixel, BIT),
    getd(Pixel, D),
    Xnew is Largo-X-1,
    pixbit(Xnew, Y, BIT, D, Pixelresultante).

%Dominio: Pixbits, número y pixbits.
%Descripción: Predicado que permite cambiar el valor de X de todo un conjunto de pixbits para FlipH.
cambiarXbits([], _, []).
cambiarXbits([Primer|Pixeles], Largo, PixelesFinales) :-
    cambiarXbit(Primer, Largo, Pixel),
    cambiarXbits(Pixeles, Largo, Pixeles2),
    my_append([Pixel], Pixeles2, PixelesFinales).

%Dominio: Pixbit, número y pixbit.
%Descripción: Predicado que permite cambiar el valor de Y de un pixbit para FlipV.
cambiarYbit(Pixel, Largo, Pixelresultante) :-
    getx(Pixel, X),
    gety(Pixel, Y),
    getbit(Pixel, BIT),
    getd(Pixel, D),
    Ynew is Largo-Y-1,
    pixbit(X, Ynew, BIT, D, Pixelresultante).  

%Dominio: Pixbit, número y pixbit.
%Descripción: Predicado que permite cambiar el valor de Y de todo un conjunto de pixbits para FlipV.
cambiarYbits([], _, []).
cambiarYbits([Primer|Pixeles], Largo, PixelesFinales) :-
    cambiarYbit(Primer, Largo, Pixel),
    cambiarYbits(Pixeles, Largo, Pixeles2),
    my_append([Pixel], Pixeles2, PixelesFinales).

%Dominio: Pixbit, cuatro números y un pixel o un -1.
%Descripción: Predicado que permite obtener si un pixel está dentro de los 4 números dados.
%             Si está dentro, se devuelve el pixel. Si no, se devuelve un -1.s
pixelAptoBit(Pixel, X1, Y1, X2, Y2, P) :-
    getx(Pixel, X),
    gety(Pixel, Y),
    (X < X1;
    X > X2;
    Y < Y1;
    Y > Y2),
    P is -1.
pixelAptoBit(Pixel, X1, Y1, X2, Y2, P) :-
    getx(Pixel, X),
    gety(Pixel, Y),
    getbit(Pixel, BIT),
    getd(Pixel, D),
    X >= X1,
    X =< X2,
    Y >= Y1,
    Y =< Y2,
    pixbit(X, Y, BIT, D, P).

%Dominio: Pixbits, cuatro números y pixbits o lista de elementos con -1.
%Descripción: Predicado que permite obtener los pixeles que están dentro del rango
%             de los cuatro números dados.
pixelesAptosBit([], _, _, _, _, []).
pixelesAptosBit([Primer|Resto], X1, Y1, X2, Y2, PixelesAptos) :-
    pixelAptoBit(Primer, X1, Y1, X2, Y2, P),
    pixelesAptosBit(Resto, X1, Y1, X2, Y2, Pixeles2),
    my_append([P], Pixeles2, PixelesAptos).

%Dominio: Pixbit, número y pixbit.
%Descripción: Predicado que permite cambiar los valores de X e Y de un pixbit debido a una rotación.
rotateBit(Pixel, Altura, Pixelresultante) :-
    getx(Pixel, X),
    gety(Pixel, Y),
    getbit(Pixel, BIT),
    getd(Pixel, D),
    Xnew is Altura - 1 - Y,

    pixbit(Xnew, X, BIT, D, Pixelresultante).

%Dominio: Pixbits, número y pixbits.
%Descripción: Predicado que permite cambiar los valores de X e Y de una serie de pixbits para realizar una rotación.
rotateBits([], _, []).
rotateBits([Primer|Pixeles], Altura, PixelesFinales) :-
    rotateBit(Primer, Altura, Pixel),
    rotateBits(Pixeles, Altura, Pixeles2),
    my_append([Pixel], Pixeles2, PixelesFinales).

%Dominio: Pixbits, color y pixbits.
%Descripción: Predicado que permite eliminar los pixbits que tengan el mismo valor que el elemento ingresado.
eliminar_dePixelesBit([], _, []).
eliminar_dePixelesBit([Primer|_], E, _) :-
    getbit(Primer, BIT),
    BIT = E,
    !.
eliminar_dePixelesBit([Primer|Pixeles], E, L) :-
    eliminar_dePixelesBit(Pixeles, E, N),
    my_append([Primer], N, L).

%Dominio: Pixeles y pixeles.
%Descripción: Predicado que permite ordenar una serie de pixeles según su valor de X.
ordenarPixelesX(List, Sorted) :-
    bubble_sortX(List, Sorted).

%Dominio: Pixeles y pixeles.
%Descripción: Predicado que permite ordenar una serie de pixeles según su valor de Y.
ordenarPixelesY(List, Sorted) :-
    bubble_sortY(List, Sorted).

%Dominio: Pixbits y string.
%Descripción: Predicado que permite transformar una fila de pixbits a string.
filasBit([], "\n").
filasBit([Pixel|Resto], String) :-
    getbit(Pixel, BIT),
    number_string(BIT, Bitazo),
    string_concat(Bitazo, '\t', String1),
    filasBit(Resto, String2),
    string_concat(String1, String2, String).

%Dominio: Pixbits y string.
%Descripción: Predicado que permite transformar todas las filas de pixbits a string.
todosBit([], "\n").
todosBit([Pixel|Resto], String) :-
    filasBit(Pixel, String1),
    todosBit(Resto, String2),
    string_concat(String1, String2, String).

%Dominio: Número, pixeles y lista de pixeles separados cada cierto número.
%Descripción: Predicado que permite separar los pixeles cada cierto número, simulando una matriz de pixeles.
separarPixelesCada(Numero, Pixeles, Resultado) :-
    length(Resultado,_),
    maplist({Numero}/[X]>>length(X,Numero),Resultado),
    append(Resultado,Pixeles).

%Dominio: Pixeles, pixeles y pixeles.
%Descripción: Predicado que permite cambiar el valor de un bit de un pixbit si es que tiene distinta profundidad con otro pixbit.
cambiarPixbits([_|_], [], []).
cambiarPixbits([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales) :-
    getx(Pixel2, X),
    gety(Pixel2, Y),
    getd(Pixel1, D1),
    getd(Pixel2, D2),
    D1 \== D2,
    pixbit(X, Y, 1, D1, P1),
    cambiarPixbits([Pixel1|Pixeles1], Pixeles2, PixelesFinales2),
    my_append([P1], PixelesFinales2, PixelesFinales).
cambiarPixbits([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales) :-
    getx(Pixel2, X),
    gety(Pixel2, Y),
    getbit(Pixel2, BIT),
    getd(Pixel1, D1),
    getd(Pixel2, D2),
    D1 = D2,
    pixbit(X, Y, BIT, D1, P1),
    cambiarPixbits([Pixel1|Pixeles1], Pixeles2, PixelesFinales2),
    my_append([P1], PixelesFinales2, PixelesFinales).

%Dominio: Pixeles, pixeles y pixeles.
%Descripción: Predicado que permite crear distintas listas de pixeles agrupados según su misma profundidad.
crearPixelesBits([], [_|_], []).
crearPixelesBits([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales) :-
    cambiarPixbits([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales1),
    crearPixelesBits(Pixeles1, [Pixel2|Pixeles2], PixelesFinales2),
    my_append([PixelesFinales1], PixelesFinales2, PixelesFinales3),
    eliminar_duplicados(PixelesFinales3, PixelesFinales).