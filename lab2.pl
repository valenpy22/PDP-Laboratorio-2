/*
X: Posición en x, entero.
Y: Posición en y, entero.
BIT: Valor del bit, entero 0 <= BIT <= 1.
D: Valor de la profundidad, entero.
*/

%CONSTRUCTORES
pixbit(X, Y, BIT, D, [X, Y, BIT, D]).

pixrgb(X, Y, R, G, B, D, [X, Y, R, G, B, D]).

pixhex(X, Y, HEX, D, [X, Y, HEX, D]).

image(W, H, [C|T], [W, H, [C|T]]) :-
    number(W),
    number(H).

%SELECTORES
x([X|Resto], X).

y([_, Y|Resto], Y).

bit([_, _, BIT|Resto], BIT).

r([_, _, R|Resto], R).

g([_, _, _, G|Resto], G).

b([_, _, _, _, B|Resto], B).

hex([_, _, Hex|Resto], Hex).

d([_, _, _, D|Resto], D).

d_rgb([_, _, _, _, _, D|Resto], D).

pixeles([W, H, Cabeza|Cola], Cabeza).

p1([Cabeza|Cola], Cabeza).
p2([Cabeza, Cabeza2|Cola], Cabeza2).
p3([Cabeza, Cabeza2, Cabeza3|Cola], Cabeza3).

w([W|_], W).

h([_, H|_], H).

isPixbit(X, Y, BIT, D) :-
    number(X),
    number(Y),
    number(BIT),
    number(D),
    (BIT is 0;
    BIT is 1).

imageIsBitmap(Imagen) :-
    pixeles(Imagen, Pixeles),

    p1(Pixeles, Pixel),
    x(Pixel, X),
    y(Pixel, Y),
    bit(Pixel, BIT),
    d(Pixel, D),

    isPixbit(X, Y, BIT, D).

isPixrgb(X, Y, R, G, B, D) :-
    number(X),
    number(Y),

    number(R),
    R >= 0,
    R =< 255,

    number(G),
    G >= 0,
    G =< 255,

    number(B),
    G >= 0,
    G =< 255,

    number(D).

imageIsPixmap(Imagen) :-
    pixeles(Imagen, Pixeles),

    p1(Pixeles, Pixel),
    x(Pixel, X),
    y(Pixel, Y),
    r(Pixel, R),
    g(Pixel, G),
    b(Pixel, B),
    d_rgb(Pixel, D),

    isPixrgb(X, Y, R, G, B, D).

isPixhex(X, Y, HEX, D) :-
    number(X),
    number(Y),
    string(HEX),
    number(D).

imageIsHexmap(Imagen) :-
    pixeles(Imagen, Pixeles),

    p1(Pixeles, Pixel),
    x(Pixel, X),
    y(Pixel, Y),
    hex(Pixel, HEX),
    d(Pixel, D),

    isPixhex(X, Y, HEX, D).

size([], 0).
size([X|Y], N) :-
    size(Y, N1),
    N is N1 + 1.

imageIsCompressed(Imagen) :-
    pixeles(Imagen, Pixeles),
    w(Imagen, W),
    h(Imagen, H),
    size(Pixeles, N),
    N =\= W*H.

append([], L2, L2).
append([H|T], L2, [H|L3]) :-
    append(T, L2, L3).

%-----------------------------------------------------------
cambiarXbit(Pixel, Largo, Pixelresultante) :-
    x(Pixel, X),
    y(Pixel, Y),
    bit(Pixel, BIT),
    d(Pixel, D),
    Xnew is Largo-X-1,
    pixbit(Xnew, Y, BIT, D, Pixelresultante).

cambiarXbits([], Largo, []).
cambiarXbits([Primer|Pixeles], Largo, PixelesFinales) :-
    cambiarXbit(Primer, Largo, Pixel),
    cambiarXbits(Pixeles, Largo, Pixeles2),
    append([Pixel], Pixeles2, PixelesFinales).
%-----------------------------------------------------------
cambiarXpix(Pixel, Largo, Pixelresultante) :-
    x(Pixel, X),
    y(Pixel, Y),
    r(Pixel, R),
    g(Pixel, G),
    b(Pixel, B),
    d_rgb(Pixel, D),
    Xnew is Largo-X-1,
    pixrgb(Xnew, Y, R, G, B, D, Pixelresultante).

cambiarXpixs([], Largo, []).
cambiarXpixs([Primer|Pixeles], Largo, PixelesFinales) :-
    cambiarXpix(Primer, Largo, Pixel),
    cambiarXpixs(Pixeles, Largo, Pixeles2),
    append([Pixel], Pixeles2, PixelesFinales).
%-----------------------------------------------------------
cambiarXhex(Pixel, Largo, Pixelresultante) :-
    x(Pixel, X),
    y(Pixel, Y),
    hex(Pixel, HEX),
    d(Pixel, D),
    Xnew is Largo-X-1,
    pixhex(Xnew, Y, HEX, D, Pixelresultante).

cambiarXhexs([], Largo, []).
cambiarXhexs([Primer|Pixeles], Largo, PixelesFinales) :-
    cambiarXhex(Primer, Largo, Pixel),
    cambiarXhexs(Pixeles, Largo, Pixeles2),
    append([Pixel], Pixeles2, PixelesFinales).
%-----------------------------------------------------------
%Por cada x cambiarlo a largo - x - 1
imageFlipH(Imagen, I2) :-
    w(Imagen, W),
    h(Imagen, H),
    pixeles(Imagen, Pixeles),
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
%-----------------------------------------------------------
cambiarYbit(Pixel, Largo, Pixelresultante) :-
    x(Pixel, X),
    y(Pixel, Y),
    bit(Pixel, BIT),
    d(Pixel, D),
    Ynew is Largo-Y-1,
    pixbit(X, Ynew, BIT, D, Pixelresultante).  

cambiarYbits([], Largo, []).
cambiarYbits([Primer|Pixeles], Largo, PixelesFinales) :-
    cambiarYbit(Primer, Largo, Pixel),
    cambiarYbits(Pixeles, Largo, Pixeles2),
    append([Pixel], Pixeles2, PixelesFinales).
%-----------------------------------------------------------
cambiarYpix(Pixel, Largo, Pixelresultante) :-
    x(Pixel, X),
    y(Pixel, Y),
    r(Pixel, R),
    g(Pixel, G),
    b(Pixel, B),
    d_rgb(Pixel, D),
    Ynew is Largo-Y-1,
    pixrgb(X, Ynew, R, G, B, D, Pixelresultante).

cambiarYpixs([], Largo, []).
cambiarYpixs([Primer|Pixeles], Largo, PixelesFinales) :-
    cambiarYpix(Primer, Largo, Pixel),
    cambiarYpixs(Pixeles, Largo, Pixeles2),
    append([Pixel], Pixeles2, PixelesFinales).
%-----------------------------------------------------------
cambiarYhex(Pixel, Largo, Pixelresultante) :-
    x(Pixel, X),
    y(Pixel, Y),
    hex(Pixel, HEX),
    d(Pixel, D),
    Ynew is Largo-Y-1,
    pixhex(X, Ynew, HEX, D, Pixelresultante).

cambiarYhexs([], Largo, []).
cambiarYhexs([Primer|Pixeles], Largo, PixelesFinales) :-
    cambiarYhex(Primer, Largo, Pixel),
    cambiarYhexs(Pixeles, Largo, Pixeles2),
    append([Pixel], Pixeles2, PixelesFinales).
%-----------------------------------------------------------
%Por cada x cambiarlo a largo - y - 1
imageFlipV(Imagen, I2) :-
    w(Imagen, W),
    h(Imagen, H),
    pixeles(Imagen, Pixeles),
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
%-----------------------------------------------------------
pixelApto(Pixel, X1, Y1, X2, Y2, P) :-
    x(Pixel, X),
    y(Pixel, Y),
    bit(Pixel, BIT),
    d(Pixel, D),

    X >= X1,
    X =< X2,
    Y >= Y1,
    Y =< Y2,
    pixbit(X, Y, BIT, D, P);
    P is -1.

pixelesAptos([], X1, Y1, X2, Y2, []).
pixelesAptos([Primer|Resto], X1, Y1, X2, Y2, PixelesAptos) :-
    pixelApto(Primer, X1, Y1, X2, Y2, P),
    pixelesAptos(Resto, X1, Y1, X2, Y2, Pixeles2),
    append([P], Pixeles2, PixelesAptos).
    
eliminar([], Elemento, []).
eliminar([E|L], E, L) :-
    !.
eliminar([C|Q], E, L) :-
    eliminar(Q, E, N),
    append([C], N, L).

imageCrop(Imagen, X1, Y1, X2, Y2, I2) :-
    w(Imagen, W),
    h(Imagen, H),
    pixeles(Imagen, Pixeles),
    pixelesAptos(Pixeles, X1, Y1, X2, Y2, PixelesAptos),
    eliminar(PixelesAptos, -1, PixelesAptosD),
    Wnew is X2-X1,
    Hnew is Y2-Y1,
    image(Wnew, Hnew, PixelesAptosD, I2).


%Crear el algoritmo que convierte de RGB a HEX.

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

pixrgbApixhex(Pixelrgb, Pixelhex) :-
    x(Pixelrgb, X),
    y(Pixelrgb, Y),
    r(Pixelrgb, R),
    g(Pixelrgb, G),
    b(Pixelrgb, B),
    d_rgb(Pixelrgb, D),
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
    append([Pixhex], PixelesFinales2, PixelesFinales).

imageRGBToHex(Imagen, I2) :-
    w(Imagen, W),
    h(Imagen, H),
    pixeles(Imagen, Pixeles),
    pixelesrgbApixeleshex(Pixeles, P),
    image(W, H, P, I2).

contarRepetidos([], Elemento, 0).
contarRepetidos([C|Q], Elemento, Numero) :-
    C = Elemento,
    contarRepetidos(Q, Elemento, N1),
    Numero is N1+1;
    contarRepetidos(Q, Elemento, Numero).

bits([], []).
bits([C|Q], N) :-
    bit(C, BIT),
    bits(Q, N2),
    append([BIT], N2, N).

rgbs([], []).
rgbs([C|Q], N) :-
    r(C, R),
    g(C, G),
    b(C, B),
    rgbs(Q, N2),
    append([[R, G, B]], N2, N).

hexs([], []).
hexs([C|Q], N) :-
    hex(C, HEX),
    hexs(Q, N2),
    append([HEX], N2, N).

histograma([], [], []).
histograma([C|Q], [Repeticion|Repeticiones], Result) :-
    histograma(Q, Repeticiones, Result2),
    append([[C, Repeticion]], Result2, Result).

contarRepetidos2([C|Q], [], []).
contarRepetidos2([C|Q], [Elemento|Elementos], N) :-
    contarRepetidos([C|Q], Elemento, N1),
    contarRepetidos2([C|Q], Elementos, N2),
    append([N1], N2, N).

eliminar_duplicados([], []).
eliminar_duplicados([Head|Tail], Result) :-
    member(Head, Tail), !,
    eliminar_duplicados(Tail, Result).
eliminar_duplicados([Head|Tail], [Head|Result]) :-
    eliminar_duplicados(Tail, Result).

imageToHistogramRGB(Imagen, Histogram) :-
    pixeles(Imagen, Pixeles),
    rgbs(Pixeles, RGBS),
    eliminar_duplicados(RGBS, Resultado1),
    contarRepetidos2(RGBS, Resultado1, N),
    histograma(Resultado1, N, Histogram).

imageToHistogramBIT(Imagen, Histogram) :-
    pixeles(Imagen, Pixeles),
    bits(Pixeles, BITS),
    eliminar_duplicados(BITS, Resultado2),
    contarRepetidos2(BITS, Resultado2, ByW),
    histograma(Resultado2, ByW, Histogram).

imageToHistogramHEX(Imagen, Histogram) :-
    pixeles(Imagen, Pixeles),
    hexs(Pixeles, HEXS),
    eliminar_duplicados(HEXS, Resultado3),
    contarRepetidos2(HEXS, Resultado3, Hex),
    histograma(Resultado3, Hex, Histogram).

imageToHistogram(Imagen, Histogram) :-
    (imageIsPixmap(Imagen),
    imageToHistogramRGB(Imagen, Histogram));

    (imageIsBitmap(Imagen),
    imageToHistogramBIT(Imagen, Histogram));

    (imageIsHexmap(Imagen),
    imageToHistogramHEX(Imagen, Histogram)).
    

rotateBit(Pixel, Altura, Pixelresultante) :-
    x(Pixel, X),
    y(Pixel, Y),
    bit(Pixel, BIT),
    d(Pixel, D),
    Xnew is Altura - 1 - Y,

    pixbit(Xnew, X, BIT, D, Pixelresultante).

rotateBits([], Altura, []).
rotateBits([Primer|Pixeles], Altura, PixelesFinales) :-
    rotateBit(Primer, Altura, Pixel),
    rotateBits(Pixeles, Altura, Pixeles2),
    append([Pixel], Pixeles2, PixelesFinales).

rotatePix(Pixel, Altura, Pixelresultante) :-
    x(Pixel, X),
    y(Pixel, Y),
    r(Pixel, R),
    g(Pixel, G),
    b(Pixel, B),
    d_rgb(Pixel, D),
    Xnew is Altura - 1 - Y,

    pixrgb(Xnew, X, R, G, B, D, Pixelresultante).

rotatePixs([], Altura, []).
rotatePixs([Primer|Pixeles], Altura, PixelesFinales) :-
    rotatePix(Primer, Altura, Pixel),
    rotatePixs(Pixeles, Altura, Pixeles2),
    append([Pixel], Pixeles2, PixelesFinales).

rotateHex(Pixel, Altura, Pixelresultante) :-
    x(Pixel, X),
    y(Pixel, Y),
    hex(Pixel, HEX),
    d(Pixel, D),
    Xnew is Altura - 1 - Y,

    pixhex(Xnew, X, HEX, D, Pixelresultante).

rotateHexs([], Altura, []).
rotateHexs([Primer|Pixeles], Altura, PixelesFinales) :-
    rotateHex(Primer, Altura, Pixel),
    rotateHexs(Pixeles, Altura, Pixeles2),
    append([Pixel], Pixeles2, PixelesFinales).

rotate90bit(Imagen, I2) :-
    w(Imagen, W),
    h(Imagen, H),
    pixeles(Imagen, Pixeles),
    rotateBits(Pixeles, H, Pixeles2),
    image(H, W, Pixeles2, I2).

rotate90pix(Imagen, I2) :-
    w(Imagen, W),
    h(Imagen, H),
    pixeles(Imagen, Pixeles),
    rotatePixs(Pixeles, H, Pixeles2),
    image(H, W, Pixeles2, I2).

rotate90hex(Imagen, I2) :-
    w(Imagen, W),
    h(Imagen, H),
    pixeles(Imagen, Pixeles),
    rotateHexs(Pixeles, H, Pixeles2),
    image(H, W, Pixeles2, I2).

rotate90(Imagen, I2) :-
    (imageIsBitmap(Imagen),
    rotate90bit(Imagen, I2));

    (imageIsPixmap(Imagen),
    rotate90pix(Imagen, I2));

    (imageIsHexmap(Imagen),
    rotate90hex(Imagen, I2)).

%Eliminar los pixeles con más frecuencia

list_max(M, [[Primer, Valor1]|Pixeles]):-
    list_max2(M, [Primer, Valor1], Pixeles).

list_max2(M, M, []):- !.

list_max2([Primer, Valor1], [Segundo, Valor2], [[Tercer, Valor3]|Pixeles2]):-
          Valor3 >= Valor2,
          !,
          list_max2([Primer, Valor1], [Tercer, Valor3], Pixeles2).

list_max2([Primer, Valor1], [Segundo, Valor2], [[Tercer, Valor3]|Pixeles2]):-
          Valor3 =< Valor2,
          list_max2([Primer, Valor1], [Segundo, Valor2], Pixeles2).


eliminar_dePixelesBit([], E, []).
eliminar_dePixelesBit([PrimerPixel|Pixeles], E, L) :-
    bit(PrimerPixel, BIT),
    BIT = E,
    !.
eliminar_dePixelesBit([Primer|Pixeles], E, L) :-
    eliminar_dePixelesBit(Pixeles, E, N),
    append([Primer], N, L).
compressBit(Imagen, I2) :-
    w(Imagen, W),
    h(Imagen, H),
    pixeles(Imagen, Pixeles),
    imageToHistogram(Imagen, Histogram),
    list_max(Mayor, Histogram),
    p1(Mayor, Primer),
    %Hacer recursión para eliminar los elementos que sean iguales al primer elemento de Mayor
    eliminar_dePixelesBit(Pixeles, Primer, Resultado),
    image(W, H, Resultado, I2).

eliminar_dePixelesHex([], E, []).
eliminar_dePixelesHex([PrimerPixel|Pixeles], E, L) :-
    hex(PrimerPixel, HEX),
    HEX = E,
    !.
eliminar_dePixelesHex([Primer|Pixeles], E, L) :-
    eliminar_dePixelesHex(Pixeles, E, N),
    append([Primer], N, L).
compressHex(Imagen, I2) :-
    w(Imagen, W),
    h(Imagen, H),
    pixeles(Imagen, Pixeles),
    imageToHistogram(Imagen, Histogram),
    list_max(Mayor, Histogram),
    p1(Mayor, Primer),
    %Hacer recursión para eliminar los elementos que sean iguales al primer elemento de Mayor
    eliminar_dePixelesHex(Pixeles, Primer, Resultado),
    image(W, H, Resultado, I2).

eliminar_dePixelesPix([], E, []).
eliminar_dePixelesPix([F|Pixeles], E, L) :-
    r(F, R),
    g(F, G),
    b(F, B),
    p1(E, Red),
    p2(E, Green),
    p3(E, Blue);
    (R = Red, G = Green, B = Blue),
    !.
eliminar_dePixelesPix([F|Pixeles], E, L) :-
    eliminar_dePixelesPix(Pixeles, E, N),
    append([Primer], N, L).
compressPix(Imagen, I2) :-
    w(Imagen, W),
    h(Imagen, H),
    pixeles(Imagen, Pixeles),
    imageToHistogram(Imagen, Histogram),
    list_max(Mayor, Histogram),
    p1(Mayor, Primer),
    eliminar_dePixelesPix(Pixeles, Primer, Resultado),
    image(W, H, Resultado, I2).

imageCompress(Imagen, I2) :-
    (imageIsBitmap(Imagen),
    compressBit(Imagen, I2));

    (imageIsPixmap(Imagen),
    compressPix(Imagen, I2));

    (imageIsHexmap(Imagen),
    compressHex(Imagen, I2)).

cambiarPixel([Pixel|Pixeles], Elemento, Pixeles2) :-
    x(Pixel, X),
    y(Pixel, Y),

    x(Elemento, X2),
    y(Elemento, Y2),

    X = X2,
    Y = Y2,
    Pixeles2 = [Elemento|Pixeles].

cambiarPixel([Pixel|Pixeles], Elemento, Pixeles3) :-
    cambiarPixel(Pixeles, Elemento, Pixeles2),
    append([Pixel], Pixeles2, Pixeles3).

imageChangePixel(Imagen, Pixel, I2) :-
    w(Imagen, W),
    h(Imagen, H),
    pixeles(Imagen, Pixeles),
    cambiarPixel(Pixeles, Pixel, Pixeles2),
    image(W, H, Pixeles2, I2).

imageInvertColorRGB(Pixel, Pixel2) :-
    x(Pixel, X),
    y(Pixel, Y),
    r(Pixel, R),
    g(Pixel, G),
    b(Pixel, B),
    d_rgb(Pixel, D),
    Rnew = 255 - R,
    Gnew = 255 - G,
    Bnew = 255 - B,
    pixrgb(X, Y, Rnew, Gnew, Bnew, D, Pixel2).

imageToString(Imagen, String).
imageDepthLayers(Imagen, ListaImagenes).

pixbit(0, 0, 1, 0, P1), pixbit(0, 1, 0, 2, P2), image(1, 2, [P1, P2], I), pixeles(I, P), pixbit(0, 0, 1, 5, P3), imageChangePixel(I, P3, I2).