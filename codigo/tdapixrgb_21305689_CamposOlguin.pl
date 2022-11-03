/*
Nombre: TDA pixrgb
Dominio:
Pixrgb: Pixrgb
- Coordenada X, Coordenada Y, Canal Red, Canal Green, Canal Blue, Profundidad: Enteros.

Predicados:
pixrgb(X, Y, R, G, B, D, PIXRGB) (aridad 7)
getx(Pixel, X) (aridad 2)
gety(Pixel, Y) (aridad 2)
extraerX(Pixeles, Xs) (aridad 2)
extraerY(Pixeles, Ys) (aridad 2)
getr(Pixrgb, Red)   (aridad 2)
getg(Pixrgb, Green) (aridad 2)
getb(Pixrgb, Blue)  (aridad 2)
getd_rgb(Pixrgb, Profundidad) (aridad 2)
rgbs(Pixrgbs, Colores) (aridad 2)
isPixrgb(X, Y, R, G, B, D)  (aridad 6)
numeroAletra(Numero, String) (aridad 2)
cambiarXpix(Pixrgb, Largo, Pixelresultante) (aridad 3)
cambiarXpixs(Pixrgbs, Largo, PixelesResultantes) (aridad 3)
cambiarYpix(Pixrgb, Largo, Pixelresultante) (aridad 3)
cambiarYpixs(Pixrgbs, Largo, PixelesResultantes) (aridad 3)
pixelAptoRGB(Pixrgb, X1, Y1, X2, Y2, Elemento) (aridad 6)
pixelesAptosRGB(Pixrgbs, X1, Y1, X2, Y2, Elementos) (aridad 6)
pixrgbApixhex(Pixrgb, Pixhex) (aridad 2)
pixelesrgbApixeleshex(Pixelesrgb, Pixhexs) (aridad 2)
rotatePix(Pixrgb, Altura, PixelFinal) (aridad 3)
rotatePixs(Pixrgbs, Altura, PixelesFinales) (aridad 3)
cambiarPorMenos(Red, Green, Blue, Pixel, Pixel2) (aridad 5)
eliminar_dePixelesPix(Pixrgbs, Red, Green, Blue, Pixelesfinales) (aridad 5)
cambiarPixel(Pixrgbs, Elemento, Pixeles2) (aridad 3)
ordenarPixelesX(Pixeles, PixelesOrdenadosX) (aridad 2)
ordenarPixelesY(Pixeles, PixelesOrdenadosY) (aridad 2)
filasRGB(Pixrgbs, String) (aridad 2)
todosRGB(Pixrgbs, String) (aridad 2)
separarPixelesCada(Numero, Pixeles, Resultado) (aridad 3)
cambiarPixrgbs(Pixrgbs, Pixrgbs2, Pixrgbs3) (aridad 3)
crearPixelesRGB(Pixrgbs, Pixrgbs2, Pixrgbs3) (aridad 3)

Metas primarias: getx, gety, getr, getg, getb, getd_rgb, isPixrgb y pixrgbApixhex.
Metas secundarias: extraerX, extraerY, rgbs, numeroAletra, cambiarXpix,
                   cambiarXpixs, cambiarYpix, cambiarYpixs, pixelAptoRGB, pixelesAptosRGB,
                   pixelesrgbApixeleshex, rotatePix, rotatePixs, cambiarPorMenos, 
                   eliminar_dePixelesPix, cambiarPixel, ordenarPixelesX, ordenarPixelesY, 
                   filasRGB, todosRGB, separarPixelesCada, cambiarPixrgbs y crearPixelesRGB.

Representación: integer X integer X integer X integer X integer X integer X list

Clausulas
- Reglas:

%Dominio: Seis números y pixrgb.
%Descripción: Predicado que permite construir el TDA pixrgb.
*/
pixrgb(X, Y, R, G, B, D, [X, Y, R, G, B, D]).

/*
SELECTORES RGB
*/

%Dominio: Un pixrgb y un número.
%Descripción: Predicado que permite obtener el valor del canal R de un pixrgb.
getr([_, _, R|_], R).

%Dominio: Un pixrgb y un número.
%Descripción: Predicado que permite obtener el valor del canal G de un pixrgb.
getg([_, _, _, G|_], G).

%Dominio: Un pixrgb y un número.
%Descripción: Predicado que permite obtener el valor del canal B de un pixrgb.
getb([_, _, _, _, B|_], B).

%Dominio: Un pixrgb y un número.
%Descripción: Predicado que permite obtener el valor de la profundidad de un pixrgb.
getd_rgb([_, _, _, _, _, D|_], D).

%Dominio: Pixrgbs y lista de colores.
%Descripción: Predicado que permite obtener los colores de una serie de pixrgbs.
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

%Dominio: Seis números.
%Descripción: Predicado que determina si un pixel es de tipo pixrgb.
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

%Dominio: Número y string.
%Descripción: Predicado que permite transformar un número decimal a valor hexadecimal en formato string.
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

%Dominio: Pixrgb, número y pixrgb.
%Descripción: Predicado que permite cambiar el valor de coordenada en X de un pixrgb para FlipH.
cambiarXpix(Pixel, Largo, Pixelresultante) :-
    getx(Pixel, X),
    gety(Pixel, Y),
    getr(Pixel, R),
    getg(Pixel, G),
    getb(Pixel, B),
    getd_rgb(Pixel, D),
    Xnew is Largo-X-1,
    pixrgb(Xnew, Y, R, G, B, D, Pixelresultante).

%Dominio: Pixrgbs, número y pixrgbs.
%Descripción: Predicado que permite cambiar los valores de las coordenadas en X de una serie de pixrgbs para FlipH.
cambiarXpixs([], _, []).
cambiarXpixs([Primer|Pixeles], Largo, PixelesFinales) :-
    cambiarXpix(Primer, Largo, Pixel),
    cambiarXpixs(Pixeles, Largo, Pixeles2),
    my_append([Pixel], Pixeles2, PixelesFinales).

%Dominio: Pixrgb, número y pixrgb.
%Descripción: Predicado que permite cambiar el valor de la coordenada en Y de un pixrgb para FlipV.
cambiarYpix(Pixel, Largo, Pixelresultante) :-
    getx(Pixel, X),
    gety(Pixel, Y),
    getr(Pixel, R),
    getg(Pixel, G),
    getb(Pixel, B),
    getd_rgb(Pixel, D),
    Ynew is Largo-Y-1,
    pixrgb(X, Ynew, R, G, B, D, Pixelresultante).

%Dominio: Pixrgb, número y pixrgb.
%Descripción: Predicado que permite cambiar los valores de las coordenadas en Y de una serie de pixrgbs para FlipV.
cambiarYpixs([], _, []).
cambiarYpixs([Primer|Pixeles], Largo, PixelesFinales) :-
    cambiarYpix(Primer, Largo, Pixel),
    cambiarYpixs(Pixeles, Largo, Pixeles2),
    my_append([Pixel], Pixeles2, PixelesFinales).

%Dominio: Pixrgb, cuatro números y un pixrgb o -1,
%Descripción: Predicado que permite obtener si un pixrgb está dentro del rango dado por los 4 números o no.
%             Si está dentro del rango, devuelve el pixrgb. Si no, devuelve un -1.
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

%Dominio: Pixrgb, cuatro números y pixrgbs o lista de -1.
%Descripción: Predicado que permite obtener los pixrgbs que están dentro del rango especificado por los 4 números.
pixelesAptosRGB([], _, _, _, _, []).
pixelesAptosRGB([Primer|Resto], X1, Y1, X2, Y2, PixelesAptos) :-
    pixelAptoRGB(Primer, X1, Y1, X2, Y2, P),
    pixelesAptosRGB(Resto, X1, Y1, X2, Y2, Pixeles2),
    my_append([P], Pixeles2, PixelesAptos).

%Dominio: Pixrgb y pixhex.
%Descripción: Predicado que permite transformar un pixrgb a pixhex.
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
    string_concat("#", H, H_cuatro),
    string_concat(H_cuatro, H_tres, HEX),

    pixhex(X, Y, HEX, D, Pixelhex).

%Dominio: Pixrgbs y pixhexs.
%Descripción: Predicado que permite transformar una serie de pixrgbs a pixhexs.
pixelesrgbApixeleshex([], []).
pixelesrgbApixeleshex([Primer|Resto], PixelesFinales) :-
    pixrgbApixhex(Primer, Pixhex),
    pixelesrgbApixeleshex(Resto, PixelesFinales2),
    my_append([Pixhex], PixelesFinales2, PixelesFinales).

%Dominio: Pixrgb, número y pixrgb.
%Descripción: Predicado que permite rotar un pixrgb.
rotatePix(Pixel, Altura, Pixelresultante) :-
    getx(Pixel, X),
    gety(Pixel, Y),
    getr(Pixel, R),
    getg(Pixel, G),
    getb(Pixel, B),
    getd_rgb(Pixel, D),
    Xnew is Altura - 1 - Y,

    pixrgb(Xnew, X, R, G, B, D, Pixelresultante).

%Dominio: Pixrgbs, número y pixrgbs.
%Descripción: Predicado que permite rotar una serie de pixrgbs.
rotatePixs([], _, []).
rotatePixs([Primer|Pixeles], Altura, PixelesFinales) :-
    rotatePix(Primer, Altura, Pixel),
    rotatePixs(Pixeles, Altura, Pixeles2),
    my_append([Pixel], Pixeles2, PixelesFinales).

%Dominio: Tres números y dos pixrgb.
%Descripción: Predicado que determina si un pixrgb tiene el mismo color que otro. Si lo tiene,
%             devuelve un -1. Si no, devuelve el mismo pixrgb.
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

%Dominio: Pixrgbs, tres números y pixrgbs.
%Descripción: Predicado que permite aplicar el predicado cambiarPorMenos a una serie de pixrgbs.
eliminar_dePixelesPix(Pixeles, Red, Green, Blue, PixelesFinales) :-
    my_maplist(cambiarPorMenos(Red, Green, Blue), Pixeles, PixelesFinales).

%Dominio: Pixrgbs, pixrgb y pixrgbs.
%Descripción: Predicado que permite actualizar un pixel dentro de muchos pixeles.
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

%Dominio: Pixrgbs y string.
%Descripción: Predicado que permite transformar una fila de pixrgbs a string.
filasRGB([], "\n").
filasRGB([Pixel|Resto], String) :-
    getr(Pixel, R),
    getg(Pixel, G),
    getb(Pixel, B),
    getd_rgb(Pixel, D),
    number_string(R, Red),
    number_string(G, Green),
    number_string(B, Blue),
    number_string(D, Depth),
    string_concat(Red, ".", String1),
    string_concat(Green, ".", String2),
    string_concat(Blue, ".", String3),
    string_concat(Depth, "\t", String90),
    
    string_concat(String1, String2, String6),
    string_concat(String6, String3, String7),
    string_concat(String7, String90, String8),

    filasRGB(Resto, String9),
    string_concat(String8, String9, String).

%Dominio: Pixrgbs y string.
%Descripción: Predicado que permite transformar una matriz de pixrgbs a string.
todosRGB([], "\n").
todosRGB([Pixel|Resto], String) :-
    filasRGB(Pixel, String1),
    todosRGB(Resto, String2),
    string_concat(String1, String2, String).

%Dominio: Pixrgbs, pixrgbs y pixrgbs.
%Descripción: Predicado que permite actualizar el color a blanco de un pixel si no tiene la misma
%             profundidad que otro pixel.
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

%Dominio: Pixrgbs, pixrgbs y pixrgbs.
%Descripción: Predicado que permite crear listas de pixrgbs agrupados según su profundidad.
crearPixelesRGB([], [_|_], []).
crearPixelesRGB([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales) :-
    cambiarPixrgbs([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales1),
    crearPixelesRGB(Pixeles1, [Pixel2|Pixeles2], PixelesFinales2),
    my_append([PixelesFinales1], PixelesFinales2, PixelesFinales3),
    eliminar_duplicados(PixelesFinales3, PixelesFinales).