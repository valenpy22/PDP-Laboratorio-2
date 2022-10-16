
/*
Dominios
pixrgb		Lista.
X       	Número >= 0.
Y      		Número >= 0.
R 			Número >= 0 and Número <= 255.
G 			Número >= 0 and Número <= 255.
B 			Número >= 0 and Número <= 255.
D       	Número.
Pixel 		pixrgb.
Pixeles 	null | pixrgb X Pixeles.
Numero 		Número >= 0 and Número < 16.
Letra 		String.
*/

/*
CONSTRUCTOR
*/
pixrgb(X, Y, R, G, B, D, [X, Y, R, G, B, D]).

/*
SELECTORES RGB
*/
getr([_, _, R|_], R).
getg([_, _, _, G|_], G).
getb([_, _, _, _, B|_], B).
getd_rgb([_, _, _, _, _, D|_], D).

rgbs([], []).
rgbs([Pixel|Pixeles], N) :-
    getr(Pixel, R),
    getg(Pixel, G),
    getb(Pixel, B),
    rgbs(Pixeles, N2),
    my_append([[R, G, B]], N2, N).

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

crearPixelesRGB([], [_|_], []).
crearPixelesRGB([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales) :-
    cambiarPixrgbs([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales1),
    crearPixelesRGB(Pixeles1, [Pixel2|Pixeles2], PixelesFinales2),
    my_append([PixelesFinales1], PixelesFinales2, PixelesFinales3),
    eliminar_duplicados(PixelesFinales3, PixelesFinales).