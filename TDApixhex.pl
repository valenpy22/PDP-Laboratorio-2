

/*
CONSTRUCTOR
*/
pixhex(X, Y, HEX, D, [X, Y, HEX, D]).

/*
SELECTORES HEX
*/
gethex([_, _, Hex|_], Hex).

hexs([], []).
hexs([C|Q], N) :-
    gethex(C, HEX),
    hexs(Q, N2),
    my_append([HEX], N2, N).

/*
PERTENENCIA PIXHEX
*/
isPixhex(X, Y, HEX, D) :-
    integer(X),
    integer(Y),
    string(HEX),
    integer(D).

/*
MODIFICADORES PIXHEX
*/
cambiarXhex(Pixel, Largo, Pixelresultante) :-
    getx(Pixel, X),
    gety(Pixel, Y),
    gethex(Pixel, HEX),
    getd(Pixel, D),
    Xnew is Largo-X-1,
    pixhex(Xnew, Y, HEX, D, Pixelresultante).

cambiarXhexs([], _, []).
cambiarXhexs([Primer|Pixeles], Largo, PixelesFinales) :-
    cambiarXhex(Primer, Largo, Pixel),
    cambiarXhexs(Pixeles, Largo, Pixeles2),
    my_append([Pixel], Pixeles2, PixelesFinales).

cambiarYhex(Pixel, Largo, Pixelresultante) :-
    getx(Pixel, X),
    gety(Pixel, Y),
    gethex(Pixel, HEX),
    getd(Pixel, D),
    Ynew is Largo-Y-1,
    pixhex(X, Ynew, HEX, D, Pixelresultante).

cambiarYhexs([], _, []).
cambiarYhexs([Primer|Pixeles], Largo, PixelesFinales) :-
    cambiarYhex(Primer, Largo, Pixel),
    cambiarYhexs(Pixeles, Largo, Pixeles2),
    my_append([Pixel], Pixeles2, PixelesFinales).

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

pixelesAptosHex([], _, _, _, _, []).
pixelesAptosHex([Primer|Resto], X1, Y1, X2, Y2, PixelesAptos) :-
    pixelAptoHex(Primer, X1, Y1, X2, Y2, P),
    pixelesAptosHex(Resto, X1, Y1, X2, Y2, Pixeles2),
    my_append([P], Pixeles2, PixelesAptos).

rotateHex(Pixel, Altura, Pixelresultante) :-
    getx(Pixel, X),
    gety(Pixel, Y),
    gethex(Pixel, HEX),
    getd(Pixel, D),
    Xnew is Altura - 1 - Y,

    pixhex(Xnew, X, HEX, D, Pixelresultante).

rotateHexs([], _, []).
rotateHexs([Primer|Pixeles], Altura, PixelesFinales) :-
    rotateHex(Primer, Altura, Pixel),
    rotateHexs(Pixeles, Altura, Pixeles2),
    my_append([Pixel], Pixeles2, PixelesFinales).

eliminar_dePixelesHex([], _, []).
eliminar_dePixelesHex([Primer|_], E, _) :-
    gethex(Primer, HEX),
    HEX = E,
    !.
eliminar_dePixelesHex([Primer|Pixeles], E, L) :-
    eliminar_dePixelesHex(Pixeles, E, N),
    my_append([Primer], N, L).

filasHex([], "\n").
filasHex([Pixel|Resto], String) :-
    gethex(Pixel, HEX),
    string_concat(HEX, "\t", String1),
    filasHex(Resto, String2),
    string_concat(String1, String2, String).

todosHex([], "\n").
todosHex([Pixel|Resto], String) :-
    filasHex(Pixel, String1),
    todosHex(Resto, String2),
    string_concat(String1, String2, String).

cambiarPixhexs([_|_], [], []).
cambiarPixhexs([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales) :-
    getx(Pixel2, X),
    gety(Pixel2, Y),
    getd(Pixel1, D1),
    getd(Pixel2, D2),
    D1 \== D2,
    pixhex(X, Y, "FFFFFF", D1, P1),
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

crearPixelesHexs([], [_|_], []).
crearPixelesHexs([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales) :-
    cambiarPixhexs([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales1),
    crearPixelesHexs(Pixeles1, [Pixel2|Pixeles2], PixelesFinales2),
    my_append([PixelesFinales1], PixelesFinales2, PixelesFinales3),
    eliminar_duplicados(PixelesFinales3, PixelesFinales).