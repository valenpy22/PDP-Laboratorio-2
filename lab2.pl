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

    pixbit(X, Y, BIT, D, P).

pixelesAptos([], X1, Y1, X2, Y2, []).
pixelesAptos([Primer|Resto], X1, Y1, X2, Y2, PixelesAptos) :-
    pixelApto(Primer, X1, Y1, X2, Y2, P),
    pixelesAptos(Resto, X1, Y1, X2, Y2, Pixeles2),
    append([P], Pixeles2, PixelesAptos).
    

imageCrop(Imagen, X1, Y1, X2, Y2, I2) :-
    w(Imagen, W),
    h(Imagen, H),
    pixeles(Imagen, Pixeles),
    pixelesAptos(Pixeles, X1, Y1, X2, Y2, PixelesAptos),
    Wnew is X2-X1,
    Hnew is Y2-Y1,
    image(Wnew, Hnew, PixelesAptos, I2).



%Crear el algoritmo que convierte de RGB a HEX.
imageRGBToHex(Imagen, I2).

imageToHistogram(Imagen, Histogram).

pixbit(0, 0, 1, 10, PA), pixbit(0, 1, 0, 20, PB), pixbit(1, 0, 0, 30, PC), pixbit(1, 1, 1, 4, PD), image(2, 2, [PA, PB, PC, PD], I), imageIsCompressed(I).