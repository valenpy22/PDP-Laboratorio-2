/*
:- use_module('TDApixbit.pl', [pixbit/5]).
:- use_module('TDApixbit.pl', [getx/2]).
:- use_module('TDApixbit.pl', [gety/2]).
:- use_module('TDApixbit.pl', [getbit/2]).
:- use_module('TDApixbit.pl', [getd/2]).
:- use_module('TDApixbit.pl', [isPixbit/4]).
*/

p1([Cabeza|_], Cabeza).
p2([_, Cabeza2|_], Cabeza2).
p3([_, _, Cabeza3|_], Cabeza3).

my_size([], 0).
my_size([_|Y], N) :-
    my_size(Y, N1),
    N is N1 + 1.

my_append([], L2, L2).
my_append([H|T], L2, [H|L3]) :-
    my_append(T, L2, L3).

eliminar([], _, []).
eliminar([E|L], E, L) :-
    !.
eliminar([C|Q], E, L) :-
    eliminar(Q, E, N),
    my_append([C], N, L).

contarRepetidos([], _, 0).
contarRepetidos([C|Q], Elemento, Numero) :-
    C = Elemento,
    contarRepetidos(Q, Elemento, N1),
    Numero is N1+1;
    contarRepetidos(Q, Elemento, Numero).

contarRepetidos2([_|_], [], []).
contarRepetidos2([C|Q], [Elemento|Elementos], N) :-
    contarRepetidos([C|Q], Elemento, N1),
    contarRepetidos2([C|Q], Elementos, N2),
    my_append([N1], N2, N).

eliminar_duplicados([], []).
eliminar_duplicados([Head|Tail], Result) :-
    member(Head, Tail), !,
    eliminar_duplicados(Tail, Result).
eliminar_duplicados([Head|Tail], [Head|Result]) :-
    eliminar_duplicados(Tail, Result).

list_max(M, [[Primer, Valor1]|Pixeles]):-
    list_max2(M, [Primer, Valor1], Pixeles).

list_max2(M, M, []):- !.

list_max2([Primer, Valor1], [_, Valor2], [[Tercer, Valor3]|Pixeles2]):-
          Valor3 >= Valor2,
          !,
          list_max2([Primer, Valor1], [Tercer, Valor3], Pixeles2).

list_max2([Primer, Valor1], [Segundo, Valor2], [[_, Valor3]|Pixeles2]):-
          Valor3 =< Valor2,
          list_max2([Primer, Valor1], [Segundo, Valor2], Pixeles2).

my_maplist(_, [], []).
my_maplist(P, [A|As], [B|Bs]) :-
    call(P, A, B),
    my_maplist(P, As, Bs).

bubble_sortX(List,Sorted):-
    b_sortX(List,[],Sorted).
b_sortX([],Acc,Acc).
b_sortX([H|T],Acc,Sorted):-
    bubbleX(H,T,NT,Max),
    b_sortX(NT,[Max|Acc],Sorted).
   
bubbleX(Pixel1, [], [], Pixel1).
bubbleX(Pixel1,[Pixel2|T],[Pixel2|NT],Max):-
    getx(Pixel1, X1),
    getx(Pixel2, X2),
    X1>X2,
    bubbleX(Pixel1,T,NT,Max).
bubbleX(Pixel1,[Pixel2|T],[Pixel1|NT],Max):-
    getx(Pixel1, X1),
    getx(Pixel2, X2),
    X1=<X2,
    bubbleX(Pixel2,T,NT,Max).

bubble_sortY(List,Sorted):-
    b_sortY(List,[],Sorted).
b_sortY([],Acc,Acc).
b_sortY([H|T],Acc,Sorted):-
    bubbleY(H,T,NT,Max),
    b_sortY(NT,[Max|Acc],Sorted).
   
bubbleY(X,[],[],X).
bubbleY(Pixel1,[Pixel2|T],[Pixel2|NT],Max):-
    gety(Pixel1, Y1),
    gety(Pixel2, Y2),
    Y1>Y2,
    getbubbleY(Pixel1,T,NT,Max).
bubbleY(Pixel1,[Pixel2|T],[Pixel1|NT],Max):-
    gety(Pixel1, Y1),
    gety(Pixel2, Y2),
    Y1=<Y2,
    bubbleY(Pixel2,T,NT,Max).

rAH2(R, H2) :-
    H2 is R mod 16.

rAH1(R, H1) :-
    rAH2(R, H2),
    H1 is H2 mod 16.

gAH4(G, H4) :-
    H4 is G mod 16.

gAH3(G, H3) :-
    gAH4(G, H4),
    H3 is H4 mod 16.

bAH6(B, H6) :-
    H6 is B mod 16.

bAH5(B, H5) :-
    bAH6(B, H6),
    H5 is H6 mod 16.

/*
CONSTRUCTORES
*/
pixbit(X, Y, BIT, D, [X, Y, BIT, D]).
pixrgb(X, Y, R, G, B, D, [X, Y, R, G, B, D]).
pixhex(X, Y, HEX, D, [X, Y, HEX, D]).
image(W, H, [C|T], [W, H, Pixeles2]) :-
    number(W),
    number(H),
    ordenarPixelesX([C|T], Pixeles),
    ordenarPixelesY(Pixeles, Pixeles2).
histograma([], [], []).
histograma([C|Q], [Repeticion|Repeticiones], Result) :-
    histograma(Q, Repeticiones, Result2),
    my_append([[C, Repeticion]], Result2, Result).

/*
SELECTORES BITS
*/
getx([X|_], X).
gety([_, Y|_], Y).
getbit([_, _, BIT|_], BIT).
getd([_, _, _, D|_], D).

bits([], []).
bits([C|Q], N) :-
    getbit(C, BIT),
    bits(Q, N2),
    my_append([BIT], N2, N).

/*
SELECTORES RGB
*/
getr([_, _, R|_], R).
getg([_, _, _, G|_], G).
getb([_, _, _, _, B|_], B).
getd_rgb([_, _, _, _, _, D|_], D).

rgbs([], []).
rgbs([C|Q], N) :-
    getr(C, R),
    getg(C, G),
    getb(C, B),
    rgbs(Q, N2),
    my_append([[R, G, B]], N2, N).

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
SELECTORES IMAGENs
*/
getw([W|_], W).
geth([_, H|_], H).
getpixeles([_, _, Cabeza|_], Cabeza).

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
PERTENENCIA PIXRGB
*/
isPixrgb(X, Y, R, G, B, D) :-
    integer(X),
    integer(Y),
    integer(R),
    R >= 0,
    R =< 255,
    integer(G),
    G >= 0,
    G =< 255,
    integer(B),
    G >= 0,
    G =< 255,
    integer(D).

numeroAletra(Numero, Letra) :-
    (Numero = 15, 
    Letra = 'F');
    (Numero = 14,
    Letra = 'E');
    (Numero = 13, 
    Letra = 'D');
    (Numero = 12,
    Letra = 'C');
    (Numero = 11, 
    Letra = 'B');
    (Numero = 10,
    Letra = 'A');
    (Numero = 9, 
    Letra = '9');
    (Numero = 8,
    Letra = '8');
    (Numero = 7, 
    Letra = '7');
    (Numero = 6,
    Letra = '6');
    (Numero = 5, 
    Letra = '5');
    (Numero = 4,
    Letra = '4');
    (Numero = 3, 
    Letra = '3');
    (Numero = 2,
    Letra = '2');
    (Numero = 1, 
    Letra = '1');
    (Numero = 0,
    Letra = '0').

/*
PERTENENCIA PIXHEX
*/
isPixhex(X, Y, HEX, D) :-
    integer(X),
    integer(Y),
    string(HEX),
    integer(D).

/*
PERTENENCIA IMAGEN
*/
imageIsBitmap(Imagen) :-
    getpixeles(Imagen, Pixeles),
    p1(Pixeles, Pixel),
    getx(Pixel, X),
    gety(Pixel, Y),
    getbit(Pixel, BIT),
    getd(Pixel, D),
    isPixbit(X, Y, BIT, D).

imageIsPixmap(Imagen) :-
    getpixeles(Imagen, Pixeles),
    p1(Pixeles, Pixel),
    getx(Pixel, X),
    gety(Pixel, Y),
    getr(Pixel, R),
    getg(Pixel, G),
    getb(Pixel, B),
    getd_rgb(Pixel, D),
    isPixrgb(X, Y, R, G, B, D).

imageIsHexmap(Imagen) :-
    getpixeles(Imagen, Pixeles),
    p1(Pixeles, Pixel),
    getx(Pixel, X),
    gety(Pixel, Y),
    gethex(Pixel, HEX),
    getd(Pixel, D),
    isPixhex(X, Y, HEX, D).

imageIsCompressed(Imagen) :-
    getpixeles(Imagen, Pixeles),
    getw(Imagen, W),
    geth(Imagen, H),
    my_size(Pixeles, N),
    N =\= W*H.

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


/*
MODIFICADORES PIXRGB
*/
cambiarXpix(Pixel, Largo, Pixelresultante) :-
    getx(Pixel, X),
    gety(Pixel, Y),
    getr(Pixel, R),
    getg(Pixel, G),
    getb(Pixel, B),
    getd_rgb(Pixel, D),
    Xnew is Largo-X-1,
    pixrgb(Xnew, Y, R, G, B, D, Pixelresultante).

cambiarXpixs([], _, []).
cambiarXpixs([Primer|Pixeles], Largo, PixelesFinales) :-
    cambiarXpix(Primer, Largo, Pixel),
    cambiarXpixs(Pixeles, Largo, Pixeles2),
    my_append([Pixel], Pixeles2, PixelesFinales).

cambiarYpix(Pixel, Largo, Pixelresultante) :-
    getx(Pixel, X),
    gety(Pixel, Y),
    getr(Pixel, R),
    getg(Pixel, G),
    getb(Pixel, B),
    getd_rgb(Pixel, D),
    Ynew is Largo-Y-1,
    pixrgb(X, Ynew, R, G, B, D, Pixelresultante).

cambiarYpixs([], _, []).
cambiarYpixs([Primer|Pixeles], Largo, PixelesFinales) :-
    cambiarYpix(Primer, Largo, Pixel),
    cambiarYpixs(Pixeles, Largo, Pixeles2),
    my_append([Pixel], Pixeles2, PixelesFinales).

pixelAptoRGB(Pixel, X1, Y1, X2, Y2, P) :-
    getx(Pixel, X),
    gety(Pixel, Y),
    (X < X1;
    X > X2;
    Y < Y1;
    Y > Y2),
    P is -1.
pixelAptoRGB(Pixel, X1, Y1, X2, Y2, P) :-
    getx(Pixel, X),
    gety(Pixel, Y),
    getr(Pixel, R),
    getg(Pixel, G),
    getb(Pixel, B),
    getd_rgb(Pixel, D),
    X >= X1,
    X =< X2,
    Y >= Y1,
    Y =< Y2,
    pixrgb(X, Y, R, G, B, D, P).

pixelesAptosRGB([], _, _, _, _, []).
pixelesAptosRGB([Primer|Resto], X1, Y1, X2, Y2, PixelesAptos) :-
    pixelAptoRGB(Primer, X1, Y1, X2, Y2, P),
    pixelesAptosRGB(Resto, X1, Y1, X2, Y2, Pixeles2),
    my_append([P], Pixeles2, PixelesAptos).

pixrgbApixhex(Pixelrgb, Pixelhex) :-
    getx(Pixelrgb, X),
    gety(Pixelrgb, Y),
    getr(Pixelrgb, R),
    getg(Pixelrgb, G),
    getb(Pixelrgb, B),
    getd_rgb(Pixelrgb, D),
    rAH1(R, R1),
    rAH2(R, R2),
    gAH3(G, G3),
    gAH4(G, G4),
    bAH5(B, B5),
    bAH6(B, B6),
    numeroAletra(R1, H1),
    numeroAletra(R2, H2),
    numeroAletra(G3, H3),
    numeroAletra(G4, H4),
    numeroAletra(B5, H5),
    numeroAletra(B6, H6),
    string_concat(H1, H2, H_uno),
    string_concat(H3, H4, H_dos),
    string_concat(H5, H6, H_tres),

    string_concat(H_uno, H_dos, H),
    string_concat(H, H_tres, HEX),

    pixhex(X, Y, HEX, D, Pixelhex).

pixelesrgbApixeleshex([], []).
pixelesrgbApixeleshex([Primer|Resto], PixelesFinales) :-
    pixrgbApixhex(Primer, Pixhex),
    pixelesrgbApixeleshex(Resto, PixelesFinales2),
    my_append([Pixhex], PixelesFinales2, PixelesFinales).

rotatePix(Pixel, Altura, Pixelresultante) :-
    getx(Pixel, X),
    gety(Pixel, Y),
    getr(Pixel, R),
    getg(Pixel, G),
    getb(Pixel, B),
    getd_rgb(Pixel, D),
    Xnew is Altura - 1 - Y,

    pixrgb(Xnew, X, R, G, B, D, Pixelresultante).

rotatePixs([], _, []).
rotatePixs([Primer|Pixeles], Altura, PixelesFinales) :-
    rotatePix(Primer, Altura, Pixel),
    rotatePixs(Pixeles, Altura, Pixeles2),
    my_append([Pixel], Pixeles2, PixelesFinales).

cambiarPorMenos(Red, Green, Blue, Pixel, Pixel2) :-
    (getr(Pixel, R),
    getg(Pixel, G),
    getb(Pixel, B)),
    (Red \== R ; Green \== G; Blue \== B),
    getx(Pixel, X),
    gety(Pixel, Y),
    getd_rgb(Pixel, D),
    pixrgb(X, Y, R, G, B, D, Pixel2).

cambiarPorMenos(Red, Green, Blue, Pixel, Y) :-
    getr(Pixel, R),
    getg(Pixel, G),
    getb(Pixel, B),
    R = Red,
    G = Green,
    B = Blue,
    Y is -1.

eliminar_dePixelesPix(Pixeles, Red, Green, Blue, PixelesFinales) :-
    my_maplist(cambiarPorMenos(Red, Green, Blue), Pixeles, PixelesFinales).

cambiarPixel([Pixel|Pixeles], Elemento, Pixeles2) :-
    getx(Pixel, X),
    gety(Pixel, Y),
    getx(Elemento, X2),
    gety(Elemento, Y2),
    X = X2,
    Y = Y2,
    Pixeles2 = [Elemento|Pixeles].
cambiarPixel([Pixel|Pixeles], Elemento, Pixeles3) :-
    cambiarPixel(Pixeles, Elemento, Pixeles2),
    my_append([Pixel], Pixeles2, Pixeles3).

imageInvertColorRGB(Pixel, Pixel2) :-
    getx(Pixel, X),
    gety(Pixel, Y),
    getr(Pixel, R),
    getg(Pixel, G),
    getb(Pixel, B),
    getd_rgb(Pixel, D),
    Rnew = 255 - R,
    Gnew = 255 - G,
    Bnew = 255 - B,
    pixrgb(X, Y, Rnew, Gnew, Bnew, D, Pixel2).

filasRGB([], "\n").
filasRGB([Pixel|Resto], String) :-
    getr(Pixel, R),
    getg(Pixel, G),
    getb(Pixel, B),
    number_string(R, Red),
    number_string(G, Green),
    number_string(B, Blue),
    string_concat(Red, "\t", String1),
    string_concat(Green, "\t", String2),
    string_concat(Blue, "\t", String3),
    string_concat(String1, String2, String4),
    string_concat(String4, String3, String5),
    filasRGB(Resto, String6),
    string_concat(String5, String6, String).

todosRGB([], "\n").
todosRGB([Pixel|Resto], String) :-
    filasRGB(Pixel, String1),
    todosRGB(Resto, String2),
    string_concat(String1, String2, String).

cambiarPixrgbs([_|_], [], []).
cambiarPixrgbs([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales) :-
    getx(Pixel2, X),
    gety(Pixel2, Y),
    getd_rgb(Pixel1, D1),
    getd_rgb(Pixel2, D2),
    D1 \== D2,
    pixrgb(X, Y, 255, 255, 255, D1, P1),
    cambiarPixrgbs([Pixel1|Pixeles1], Pixeles2, PixelesFinales2),
    my_append([P1], PixelesFinales2, PixelesFinales).
cambiarPixrgbs([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales) :-
    getx(Pixel2, X),
    gety(Pixel2, Y),
    getr(Pixel2, R),
    getg(Pixel2, G),
    getb(Pixel2, B),
    getd_rgb(Pixel1, D1),
    getd_rgb(Pixel2, D2),
    D1 = D2,
    pixrgb(X, Y, R, G, B, D1, P1),
    cambiarPixrgbs([Pixel1|Pixeles1], Pixeles2, PixelesFinales2),
    my_append([P1], PixelesFinales2, PixelesFinales).

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

/*
MODIFICADORES IMAGEN
*/

separarPixelesCada(Numero,Pixeles,Resultado) :-
    length(Resultado,_),
    maplist({Numero}/[X]>>length(X,Numero),Resultado),
    my_append(Resultado,Pixeles).

crearPixelesBits([], [_|_], []).
crearPixelesBits([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales) :-
    crearImgBit([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales1),
    crearPixelesBits(Pixeles1, [Pixel2|Pixeles2], PixelesFinales2),
    my_append([PixelesFinales1], PixelesFinales2, PixelesFinales3),
    eliminar_duplicados(PixelesFinales3, PixelesFinales).

crearPixelesRGB([], [_|_], []).
crearPixelesRGB([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales) :-
    cambiarPixrgbs([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales1),
    crearPixelesRGB(Pixeles1, [Pixel2|Pixeles2], PixelesFinales2),
    my_append([PixelesFinales1], PixelesFinales2, PixelesFinales3),
    eliminar_duplicados(PixelesFinales3, PixelesFinales).

crearPixelesHexs([], [_|_], []).
crearPixelesHexs([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales) :-
    cambiarPixhexs([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales1),
    crearPixelesHexs(Pixeles1, [Pixel2|Pixeles2], PixelesFinales2),
    my_append([PixelesFinales1], PixelesFinales2, PixelesFinales3),
    eliminar_duplicados(PixelesFinales3, PixelesFinales).

crearImagenes(_, [], []).
crearImagenes(Imagen, [Pixeles1|Pixeles2], ListaImagenes) :-
    getw(Imagen, W),
    geth(Imagen, H),
    image(W, H, Pixeles1, I1),
    crearImagenes(Imagen, Pixeles2, ListaImagenes2),
    my_append([I1], ListaImagenes2, ListaImagenes).

%-----------------------------------------------------------------------
imageFlipH(Imagen, I2) :-
    getw(Imagen, W),
    geth(Imagen, H),
    getpixeles(Imagen, Pixeles),
    (
    imageIsBitmap(Imagen),
    cambiarXbits(Pixeles, W, Pixeles2),
    image(W, H, Pixeles2, I2);

    imageIsPixmap(Imagen),
    cambiarXpixs(Pixeles, W, Pixeles3),
    image(W, H, Pixeles3, I2); 

    imageIsHexmap(Imagen),
    cambiarXhexs(Pixeles, W, Pixeles4),
    image(W, H, Pixeles4, I2)).

imageFlipV(Imagen, I2) :-
    getw(Imagen, W),
    geth(Imagen, H),
    getpixeles(Imagen, Pixeles),
    (
    imageIsBitmap(Imagen),
    cambiarYbits(Pixeles, H, Pixeles2),
    image(W, H, Pixeles2, I2);

    imageIsPixmap(Imagen),
    cambiarYpixs(Pixeles, H, Pixeles3),
    image(W, H, Pixeles3, I2); 

    imageIsHexmap(Imagen),
    cambiarYhexs(Pixeles, H, Pixeles4),
    image(W, H, Pixeles4, I2)).

%SERÁ MEJOR SACAR EL NÚMERO MAYOR DE LOS X, EL NÚMERO MENOR DE LOS X Y LO MISMO CON LOS Y PARA DETERMINAR LA NUEVA DIMENSIÓN.
imageCrop(Imagen, X1, Y1, X2, Y2, I2) :-
    Wnew is X2-X1 + 1,
    Hnew is Y2-Y1 + 1,
    getpixeles(Imagen, Pixeles),
    ((imageIsBitmap(Imagen),
    pixelesAptosBit(Pixeles, X1, Y1, X2, Y2, PixelesAptos1),
    exclude(integer, PixelesAptos1, PixelesAptos));
    (imageIsPixmap(Imagen),
    pixelesAptosRGB(Pixeles, X1, Y1, X2, Y2, PixelesAptos2),
    exclude(integer, PixelesAptos2, PixelesAptos));
    (imageIsHexmap(Imagen),
    pixelesAptosHex(Pixeles, X1, Y1, X2, Y2, PixelesAptos3),
    exclude(integer, PixelesAptos3, PixelesAptos))),
    image(Wnew, Hnew, PixelesAptos, I2).

imageRGBToHex(Imagen, I2) :-
    getw(Imagen, W),
    geth(Imagen, H),
    getpixeles(Imagen, Pixeles),
    pixelesrgbApixeleshex(Pixeles, P),
    image(W, H, P, I2).

imageToHistogram(Imagen, Histogram) :-
    getpixeles(Imagen, Pixeles),
    ((imageIsBitmap(Imagen),
    getbits(Pixeles, BITS),
    eliminar_duplicados(BITS, Resultado2),
    contarRepetidos2(BITS, Resultado2, ByW),
    histograma(Resultado2, ByW, Histogram));

    (imageIsPixmap(Imagen),
    rgbs(Pixeles, RGBS),
    eliminar_duplicados(RGBS, Resultado1),
    contarRepetidos2(RGBS, Resultado1, N),
    histograma(Resultado1, N, Histogram));

    (imageIsHexmap(Imagen),
    gethexs(Pixeles, HEXS),
    eliminar_duplicados(HEXS, Resultado3),
    contarRepetidos2(HEXS, Resultado3, Hex),
    histograma(Resultado3, Hex, Histogram))).

imageRotate90(Imagen, I2) :-
    getw(Imagen, W),
    geth(Imagen, H),
    getpixeles(Imagen, Pixeles),
    ((imageIsBitmap(Imagen),
    rotateBits(Pixeles, H, Pixeles2),
    image(H, W, Pixeles2, I2));

    (imageIsPixmap(Imagen),
    rotatePixs(Pixeles, H, Pixeles2),
    image(H, W, Pixeles2, I2));

    (imageIsHexmap(Imagen),
    rotateHexs(Pixeles, H, Pixeles2),
    image(H, W, Pixeles2, I2))).

imageCompress(Imagen, I2) :-
    getw(Imagen, W),
    geth(Imagen, H),
    getpixeles(Imagen, Pixeles),
    imageToHistogram(Imagen, Histogram),
    list_max(Mayor, Histogram),
    p1(Mayor, Primer),

    ((imageIsBitmap(Imagen),
    eliminar_dePixelesBit(Pixeles, Primer, Resultado),
    image(W, H, Resultado, I2));

    (imageIsPixmap(Imagen),
    p1(Primer, Red),
    p2(Primer, Green),
    p3(Primer, Blue),
    eliminar_dePixelesPix(Pixeles, Red, Green, Blue, Resultado),
    exclude(integer, Resultado, Final2),
    image(W, H, Final2, I2));

    (imageIsHexmap(Imagen),
    eliminar_dePixelesHex(Pixeles, Primer, Resultado),
    image(W, H, Resultado, I2))).

imageChangePixel(Imagen, Pixel, I2) :-
    getw(Imagen, W),
    geth(Imagen, H),
    getpixeles(Imagen, Pixeles),
    cambiarPixel(Pixeles, Pixel, Pixeles2),
    image(W, H, Pixeles2, I2).

imageToString(Imagen, String) :-
    getw(Imagen, W),
    getpixeles(Imagen, Pixeles),
    separarPixelesCada(W, Pixeles, PixelesSeparados),
    ((imageIsBitmap(Imagen),
    todosBit(PixelesSeparados, String));
    (imageIsPixmap(Imagen),
    todosRGB(PixelesSeparados, String));
    (imageIsHexmap(Imagen),
    todosHex(PixelesSeparados, String))).

imageDepthLayers(Imagen, ListaImagenes) :-
    getpixeles(Imagen, Pixeles),
    ((imageIsBitmap(Imagen),
    crearPixelesBits(Pixeles, Pixeles, P),
    crearImagenes(Imagen, P, ListaImagenes));

    (imageIsPixmap(Imagen),
    crearPixelesRGB(Pixeles, Pixeles, P1),
    crearImagenes(Imagen, P1, ListaImagenes));

    (imageIsHexmap(Imagen),
    crearPixelesHexs(Pixeles, Pixeles, P2),
    crearImagenes(Imagen, P2, ListaImagenes))).
