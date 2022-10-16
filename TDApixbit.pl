:- module(pixbit, [pixbit/5]).
:- module(getx, [getx/2]).
:- module(gety, [gety/2]).
:- module(getbit, [getbit/2]).
:- module(getd, [getd/2]).
:- module(isPixbit, [isPixbit/4]).

/*
Dominios
pixbit              Lista.
X                   Número >= 0.
Y                   Número >= 0.
BIT                 Número = [0|1].
D                   Número.
Pixel               Lista.
Pixeles             Lista.
*/

/*
CONSTRUCTOR
*/

pixbit(X, Y, BIT, D, [X, Y, BIT, D]).

/*
SELECTORES BITS
*/
getx([X|_], X).
gety([_, Y|_], Y).
getbit([_, _, BIT|_], BIT).
getd([_, _, _, D|_], D).

bits([], []).
bits([Pixel|Pixeles], N) :-
    getbit(Pixel, BIT),
    bits(Pixeles, N2),
    my_append([BIT], N2, N).

extraerX([], []).
extraerX([Pixel|Pixeles], N) :-
    getx(Pixel, X),
    extraerX(Pixeles, N2),
    my_append([X], N2, N).

extraerY([], []).
extraerY([Pixel|Pixeles], N) :-
    gety(Pixel, Y),
    extraerY(Pixeles, N2),
    my_append([Y], N2, N).

/*
PERTENENCIA PIXBIT
*/
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
cambiarXbit(Pixel, Largo, Pixelresultante) :-
    getx(Pixel, X),
    gety(Pixel, Y),
    getbit(Pixel, BIT),
    getd(Pixel, D),
    Xnew is Largo-X-1,
    pixbit(Xnew, Y, BIT, D, Pixelresultante).

cambiarXbits([], _, []).
cambiarXbits([Primer|Pixeles], Largo, PixelesFinales) :-
    cambiarXbit(Primer, Largo, Pixel),
    cambiarXbits(Pixeles, Largo, Pixeles2),
    my_append([Pixel], Pixeles2, PixelesFinales).

cambiarYbit(Pixel, Largo, Pixelresultante) :-
    getx(Pixel, X),
    gety(Pixel, Y),
    getbit(Pixel, BIT),
    getd(Pixel, D),
    Ynew is Largo-Y-1,
    pixbit(X, Ynew, BIT, D, Pixelresultante).  

cambiarYbits([], _, []).
cambiarYbits([Primer|Pixeles], Largo, PixelesFinales) :-
    cambiarYbit(Primer, Largo, Pixel),
    cambiarYbits(Pixeles, Largo, Pixeles2),
    my_append([Pixel], Pixeles2, PixelesFinales).

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

pixelesAptosBit([], _, _, _, _, []).
pixelesAptosBit([Primer|Resto], X1, Y1, X2, Y2, PixelesAptos) :-
    pixelAptoBit(Primer, X1, Y1, X2, Y2, P),
    pixelesAptosBit(Resto, X1, Y1, X2, Y2, Pixeles2),
    my_append([P], Pixeles2, PixelesAptos).

rotateBit(Pixel, Altura, Pixelresultante) :-
    getx(Pixel, X),
    gety(Pixel, Y),
    getbit(Pixel, BIT),
    getd(Pixel, D),
    Xnew is Altura - 1 - Y,

    pixbit(Xnew, X, BIT, D, Pixelresultante).

rotateBits([], _, []).
rotateBits([Primer|Pixeles], Altura, PixelesFinales) :-
    rotateBit(Primer, Altura, Pixel),
    rotateBits(Pixeles, Altura, Pixeles2),
    my_append([Pixel], Pixeles2, PixelesFinales).

eliminar_dePixelesBit([], _, []).
eliminar_dePixelesBit([Primer|_], E, _) :-
    getbit(Primer, BIT),
    BIT = E,
    !.
eliminar_dePixelesBit([Primer|Pixeles], E, L) :-
    eliminar_dePixelesBit(Pixeles, E, N),
    my_append([Primer], N, L).

ordenarPixelesX(List, Sorted) :-
    bubble_sortX(List, Sorted).

ordenarPixelesY(List, Sorted) :-
    bubble_sortY(List, Sorted).

filasBit([], "\n").
filasBit([Pixel|Resto], String) :-
    getbit(Pixel, BIT),
    number_string(BIT, Bitazo),
    string_concat(Bitazo, '\t', String1),
    filasBit(Resto, String2),
    string_concat(String1, String2, String).

todosBit([], "\n").
todosBit([Pixel|Resto], String) :-
    filasBit(Pixel, String1),
    todosBit(Resto, String2),
    string_concat(String1, String2, String).

separarPixelesCada(Numero,Pixeles,Resultado) :-
    length(Resultado,_),
    maplist({Numero}/[X]>>length(X,Numero),Resultado),
    my_append(Resultado,Pixeles).

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

crearPixelesBits([], [_|_], []).
crearPixelesBits([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales) :-
    cambiarPixbits([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales1),
    crearPixelesBits(Pixeles1, [Pixel2|Pixeles2], PixelesFinales2),
    my_append([PixelesFinales1], PixelesFinales2, PixelesFinales3),
    eliminar_duplicados(PixelesFinales3, PixelesFinales).
