pixbit(X, Y, BIT, D, [X, Y, BIT, D]).

pixrgb(X, Y, R, G, B, D, [X, Y, R, G, B, D]).

pixhex(X, Y, HEX, D, [X, Y, HEX, D]).

image(W, H, [C|T], [W, H, Pixeles2]) :-
    number(W),
    number(H),
    ordenarPixelesX([C|T], Pixeles),
    ordenarPixelesY(Pixeles, Pixeles2).

%SELECTORES
x([X|_], X).

y([_, Y|_], Y).

bit([_, _, BIT|_], BIT).

r([_, _, R|_], R).

g([_, _, _, G|_], G).

b([_, _, _, _, B|_], B).

hex([_, _, Hex|_], Hex).

d([_, _, _, D|_], D).

d_rgb([_, _, _, _, _, D|_], D).

pixeles([_, _, Cabeza|_], Cabeza).

p1([Cabeza|_], Cabeza).
p2([_, Cabeza2|_], Cabeza2).
p3([_, _, Cabeza3|_], Cabeza3).

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
size([_|Y], N) :-
    size(Y, N1),
    N is N1 + 1.

imageIsCompressed(Imagen) :-
    pixeles(Imagen, Pixeles),
    w(Imagen, W),
    h(Imagen, H),
    size(Pixeles, N),
    N =\= W*H.

my_append([], L2, L2).
my_append([H|T], L2, [H|L3]) :-
    my_append(T, L2, L3).

%-----------------------------------------------------------
cambiarXbit(Pixel, Largo, Pixelresultante) :-
    x(Pixel, X),
    y(Pixel, Y),
    bit(Pixel, BIT),
    d(Pixel, D),
    Xnew is Largo-X-1,
    pixbit(Xnew, Y, BIT, D, Pixelresultante).

cambiarXbits([], _, []).
cambiarXbits([Primer|Pixeles], Largo, PixelesFinales) :-
    cambiarXbit(Primer, Largo, Pixel),
    cambiarXbits(Pixeles, Largo, Pixeles2),
    my_append([Pixel], Pixeles2, PixelesFinales).
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

cambiarXpixs([], _, []).
cambiarXpixs([Primer|Pixeles], Largo, PixelesFinales) :-
    cambiarXpix(Primer, Largo, Pixel),
    cambiarXpixs(Pixeles, Largo, Pixeles2),
    my_append([Pixel], Pixeles2, PixelesFinales).
%-----------------------------------------------------------
cambiarXhex(Pixel, Largo, Pixelresultante) :-
    x(Pixel, X),
    y(Pixel, Y),
    hex(Pixel, HEX),
    d(Pixel, D),
    Xnew is Largo-X-1,
    pixhex(Xnew, Y, HEX, D, Pixelresultante).

cambiarXhexs([], _, []).
cambiarXhexs([Primer|Pixeles], Largo, PixelesFinales) :-
    cambiarXhex(Primer, Largo, Pixel),
    cambiarXhexs(Pixeles, Largo, Pixeles2),
    my_append([Pixel], Pixeles2, PixelesFinales).
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

cambiarYbits([], _, []).
cambiarYbits([Primer|Pixeles], Largo, PixelesFinales) :-
    cambiarYbit(Primer, Largo, Pixel),
    cambiarYbits(Pixeles, Largo, Pixeles2),
    my_append([Pixel], Pixeles2, PixelesFinales).
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

cambiarYpixs([], _, []).
cambiarYpixs([Primer|Pixeles], Largo, PixelesFinales) :-
    cambiarYpix(Primer, Largo, Pixel),
    cambiarYpixs(Pixeles, Largo, Pixeles2),
    my_append([Pixel], Pixeles2, PixelesFinales).
%-----------------------------------------------------------
cambiarYhex(Pixel, Largo, Pixelresultante) :-
    x(Pixel, X),
    y(Pixel, Y),
    hex(Pixel, HEX),
    d(Pixel, D),
    Ynew is Largo-Y-1,
    pixhex(X, Ynew, HEX, D, Pixelresultante).

cambiarYhexs([], _, []).
cambiarYhexs([Primer|Pixeles], Largo, PixelesFinales) :-
    cambiarYhex(Primer, Largo, Pixel),
    cambiarYhexs(Pixeles, Largo, Pixeles2),
    my_append([Pixel], Pixeles2, PixelesFinales).
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

pixelesAptos([], _, _, _, _, []).
pixelesAptos([Primer|Resto], X1, Y1, X2, Y2, PixelesAptos) :-
    pixelApto(Primer, X1, Y1, X2, Y2, P),
    pixelesAptos(Resto, X1, Y1, X2, Y2, Pixeles2),
    my_append([P], Pixeles2, PixelesAptos).
    
eliminar([], _, []).
eliminar([E|L], E, L) :-
    !.
eliminar([C|Q], E, L) :-
    eliminar(Q, E, N),
    my_append([C], N, L).

imageCrop(Imagen, X1, Y1, X2, Y2, I2) :-
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
    my_append([Pixhex], PixelesFinales2, PixelesFinales).

imageRGBToHex(Imagen, I2) :-
    w(Imagen, W),
    h(Imagen, H),
    pixeles(Imagen, Pixeles),
    pixelesrgbApixeleshex(Pixeles, P),
    image(W, H, P, I2).

contarRepetidos([], _, 0).
contarRepetidos([C|Q], Elemento, Numero) :-
    C = Elemento,
    contarRepetidos(Q, Elemento, N1),
    Numero is N1+1;
    contarRepetidos(Q, Elemento, Numero).

bits([], []).
bits([C|Q], N) :-
    bit(C, BIT),
    bits(Q, N2),
    my_append([BIT], N2, N).

rgbs([], []).
rgbs([C|Q], N) :-
    r(C, R),
    g(C, G),
    b(C, B),
    rgbs(Q, N2),
    my_append([[R, G, B]], N2, N).

hexs([], []).
hexs([C|Q], N) :-
    hex(C, HEX),
    hexs(Q, N2),
    my_append([HEX], N2, N).

histograma([], [], []).
histograma([C|Q], [Repeticion|Repeticiones], Result) :-
    histograma(Q, Repeticiones, Result2),
    my_append([[C, Repeticion]], Result2, Result).

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

rotateBits([], _, []).
rotateBits([Primer|Pixeles], Altura, PixelesFinales) :-
    rotateBit(Primer, Altura, Pixel),
    rotateBits(Pixeles, Altura, Pixeles2),
    my_append([Pixel], Pixeles2, PixelesFinales).

rotatePix(Pixel, Altura, Pixelresultante) :-
    x(Pixel, X),
    y(Pixel, Y),
    r(Pixel, R),
    g(Pixel, G),
    b(Pixel, B),
    d_rgb(Pixel, D),
    Xnew is Altura - 1 - Y,

    pixrgb(Xnew, X, R, G, B, D, Pixelresultante).

rotatePixs([], _, []).
rotatePixs([Primer|Pixeles], Altura, PixelesFinales) :-
    rotatePix(Primer, Altura, Pixel),
    rotatePixs(Pixeles, Altura, Pixeles2),
    my_append([Pixel], Pixeles2, PixelesFinales).

rotateHex(Pixel, Altura, Pixelresultante) :-
    x(Pixel, X),
    y(Pixel, Y),
    hex(Pixel, HEX),
    d(Pixel, D),
    Xnew is Altura - 1 - Y,

    pixhex(Xnew, X, HEX, D, Pixelresultante).

rotateHexs([], _, []).
rotateHexs([Primer|Pixeles], Altura, PixelesFinales) :-
    rotateHex(Primer, Altura, Pixel),
    rotateHexs(Pixeles, Altura, Pixeles2),
    my_append([Pixel], Pixeles2, PixelesFinales).

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

list_max2([Primer, Valor1], [_, Valor2], [[Tercer, Valor3]|Pixeles2]):-
          Valor3 >= Valor2,
          !,
          list_max2([Primer, Valor1], [Tercer, Valor3], Pixeles2).

list_max2([Primer, Valor1], [Segundo, Valor2], [[_, Valor3]|Pixeles2]):-
          Valor3 =< Valor2,
          list_max2([Primer, Valor1], [Segundo, Valor2], Pixeles2).


eliminar_dePixelesBit([], _, []).
eliminar_dePixelesBit([Primer|_], E, _) :-
    bit(Primer, BIT),
    BIT = E,
    !.
eliminar_dePixelesBit([Primer|Pixeles], E, L) :-
    eliminar_dePixelesBit(Pixeles, E, N),
    my_append([Primer], N, L).
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

eliminar_dePixelesHex([], _, []).
eliminar_dePixelesHex([Primer|_], E, _) :-
    hex(Primer, HEX),
    HEX = E,
    !.
eliminar_dePixelesHex([Primer|Pixeles], E, L) :-
    eliminar_dePixelesHex(Pixeles, E, N),
    my_append([Primer], N, L).
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

eliminar_dePixelesPix([], _,_,_, []).

eliminar_dePixelesPix([Primer|_], Rojo, Verde, Azul, _) :-
    r(Primer, R), 
    g(Primer, G),
    b(Primer, B),
    R = Rojo,
    G = Verde,
    B = Azul, 
    !.


eliminar_dePixelesPix([Primer|Pixeles], Rojo, Verde, Azul, L) :-
    eliminar_dePixelesPix(Pixeles, Rojo, Verde, Azul, N),
    my_append([Primer], N, L).

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
    my_append([Pixel], Pixeles2, Pixeles3).

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

%Falta mejorar el compressPix ya que no borra adecuadamente
%los pixeles según un elemento, crear un algoritmo que me permita 
%ordenar los pixeles procurando que sigan el orden
%de 0,0   1,0   2,0
%   0,1   1,1   2,1
%   0,2   1,2   2,2
%Para poder crear adecuadamente el imageToString y el DepthLayers.

bubble_sortY(List,Sorted):-
    b_sortY(List,[],Sorted).
b_sortY([],Acc,Acc).
b_sortY([H|T],Acc,Sorted):-
    bubbleY(H,T,NT,Max),
    b_sortY(NT,[Max|Acc],Sorted).
   
bubbleY(X,[],[],X).
bubbleY(Pixel1,[Pixel2|T],[Pixel2|NT],Max):-
    y(Pixel1, Y1),
    y(Pixel2, Y2),
    Y1>Y2,
    bubbleY(Pixel1,T,NT,Max).
bubbleY(Pixel1,[Pixel2|T],[Pixel1|NT],Max):-
    y(Pixel1, Y1),
    y(Pixel2, Y2),
    Y1=<Y2,
    bubbleY(Pixel2,T,NT,Max).

ordenarPixelesY(List, Sorted) :-
    bubble_sortY(List, Sorted).

bubble_sortX(List,Sorted):-
    b_sortX(List,[],Sorted).
b_sortX([],Acc,Acc).
b_sortX([H|T],Acc,Sorted):-
    bubbleX(H,T,NT,Max),
    b_sortX(NT,[Max|Acc],Sorted).
   
bubbleX(X,[],[],X).
bubbleX(Pixel1,[Pixel2|T],[Pixel2|NT],Max):-
    x(Pixel1, X1),
    x(Pixel2, X2),
    X1>X2,
    bubbleX(Pixel1,T,NT,Max).
bubbleX(Pixel1,[Pixel2|T],[Pixel1|NT],Max):-
    x(Pixel1, X1),
    x(Pixel2, X2),
    X1=<X2,
    bubbleX(Pixel2,T,NT,Max).

ordenarPixelesX(List, Sorted) :-
    bubble_sortX(List, Sorted).

separarPixelesCada(Numero,Pixeles,Resultado) :-
    length(Resultado,_),
    maplist({Numero}/[X]>>length(X,Numero),Resultado),
    my_append(Resultado,Pixeles).

filasBit([], "\n").
filasBit([Pixel|Resto], String) :-
    bit(Pixel, BIT),
    number_string(BIT, Bitazo),
    string_concat(Bitazo, '\t', String1),
    filasBit(Resto, String2),
    string_concat(String1, String2, String).

todosBit([], "\n").
todosBit([Pixel|Resto], String) :-
    filasBit(Pixel, String1),
    todosBit(Resto, String2),
    string_concat(String1, String2, String).

filasHex([], "\n").
filasHex([Pixel|Resto], String) :-
    hex(Pixel, HEX),
    string_concat(HEX, "\t", String1),
    filasHex(Resto, String2),
    string_concat(String1, String2, String).

todosHex([], "\n").
todosHex([Pixel|Resto], String) :-
    filasHex(Pixel, String1),
    todosHex(Resto, String2),
    string_concat(String1, String2, String).

filasRGB([], "\n").
filasRGB([Pixel|Resto], String) :-
    r(Pixel, R),
    g(Pixel, G),
    b(Pixel, B),
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

imageToString(Imagen, String) :-
    w(Imagen, W),
    pixeles(Imagen, Pixeles),
    separarPixelesCada(W, Pixeles, PixelesSeparados),
    ((imageIsBitmap(Imagen),
    todosBit(PixelesSeparados, String));
    (imageIsPixmap(Imagen),
    todosRGB(PixelesSeparados, String));
    (imageIsHexmap(Imagen),
    todosHex(PixelesSeparados, String))).

crearImgBit([_|_], [], []).
crearImgBit([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales) :-
    x(Pixel2, X),
    y(Pixel2, Y),
    d(Pixel1, D1),
    d(Pixel2, D2),
    D1 \== D2,
    pixbit(X, Y, 1, D1, P1),
    crearImgBit([Pixel1|Pixeles1], Pixeles2, PixelesFinales2),
    my_append([P1], PixelesFinales2, PixelesFinales).
crearImgBit([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales) :-
    x(Pixel2, X),
    y(Pixel2, Y),
    bit(Pixel2, BIT),
    d(Pixel1, D1),
    d(Pixel2, D2),
    D1 = D2,
    pixbit(X, Y, BIT, D1, P1),
    crearImgBit([Pixel1|Pixeles1], Pixeles2, PixelesFinales2),
    my_append([P1], PixelesFinales2, PixelesFinales).

crearListaImgBit([], [_|_], []).
crearListaImgBit([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales) :-
    crearImgBit([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales1),
    crearListaImgBit(Pixeles1, [Pixel2|Pixeles2], PixelesFinales2),
    my_append([PixelesFinales1], PixelesFinales2, PixelesFinales3),
    eliminar_duplicados(PixelesFinales3, PixelesFinales).

crearImgHex([_|_], [], []).
crearImgHex([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales) :-
    x(Pixel2, X),
    y(Pixel2, Y),
    d(Pixel1, D1),
    d(Pixel2, D2),
    D1 \== D2,
    pixhex(X, Y, "FFFFFF", D1, P1),
    crearImgHex([Pixel1|Pixeles1], Pixeles2, PixelesFinales2),
    my_append([P1], PixelesFinales2, PixelesFinales).
crearImgHex([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales) :-
    x(Pixel2, X),
    y(Pixel2, Y),
    hex(Pixel2, HEX),
    d(Pixel1, D1),
    d(Pixel2, D2),
    D1 = D2,
    pixhex(X, Y, HEX, D1, P1),
    crearImgHex([Pixel1|Pixeles1], Pixeles2, PixelesFinales2),
    my_append([P1], PixelesFinales2, PixelesFinales).

crearListaImgHex([], [_|_], []).
crearListaImgHex([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales) :-
    crearImgHex([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales1),
    crearListaImgHex(Pixeles1, [Pixel2|Pixeles2], PixelesFinales2),
    my_append([PixelesFinales1], PixelesFinales2, PixelesFinales3),
    eliminar_duplicados(PixelesFinales3, PixelesFinales).


crearImgRGB([_|_], [], []).
crearImgRGB([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales) :-
    x(Pixel2, X),
    y(Pixel2, Y),
    d_rgb(Pixel1, D1),
    d_rgb(Pixel2, D2),
    D1 \== D2,
    pixrgb(X, Y, 255, 255, 255, D1, P1),
    crearImgRGB([Pixel1|Pixeles1], Pixeles2, PixelesFinales2),
    my_append([P1], PixelesFinales2, PixelesFinales).

crearImgRGB([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales) :-
    x(Pixel2, X),
    y(Pixel2, Y),
    r(Pixel2, R),
    g(Pixel2, G),
    b(Pixel2, B),
    d_rgb(Pixel1, D1),
    d_rgb(Pixel2, D2),
    D1 = D2,
    pixrgb(X, Y, R, G, B, D1, P1),
    crearImgRGB([Pixel1|Pixeles1], Pixeles2, PixelesFinales2),
    my_append([P1], PixelesFinales2, PixelesFinales).

crearListaImgRGB([], [_|_], []).
crearListaImgRGB([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales) :-
    crearImgRGB([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales1),
    crearListaImgRGB(Pixeles1, [Pixel2|Pixeles2], PixelesFinales2),
    my_append([PixelesFinales1], PixelesFinales2, PixelesFinales3),
    eliminar_duplicados(PixelesFinales3, PixelesFinales).

crearImagenes(_, [], []).
crearImagenes(Imagen, [Pixeles1|Pixeles2], ListaImagenes) :-
    w(Imagen, W),
    h(Imagen, H),
    image(W, H, Pixeles1, I1),
    crearImagenes(Imagen, Pixeles2, ListaImagenes2),
    my_append([I1], ListaImagenes2, ListaImagenes).

imageDepthLayers(Imagen, ListaImagenes) :-
    pixeles(Imagen, Pixeles),
    ((imageIsBitmap(Imagen),
    crearListaImgBit(Pixeles, Pixeles, P),
    crearImagenes(Imagen, P, ListaImagenes));
    (imageIsPixmap(Imagen),
    crearListaImgRGB(Pixeles, Pixeles, P1),
    crearImagenes(Imagen, P1, ListaImagenes));
    (imageIsHexmap(Imagen),
    crearListaImgHex(Pixeles, Pixeles, P2),
    crearImagenes(Imagen, P2, ListaImagenes))).