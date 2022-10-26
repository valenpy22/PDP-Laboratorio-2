
/*
Predicados
p1(Lista, PrimerElemento)  (aridad 2)
p2(Lista, SegundoElemento) (aridad 2)
p3(Lista, TercerElemento)  (aridad 2)
my_size(Lista, Largo) (aridad 2)
my_append(Lista, Lista2, ListaResultante)  (aridad 3)
contarRepetidos(Lista, Elemento, Cantidad) (aridad 3)
contarRepetidos2(Lista, ListaElementos, ListaResultante) (aridad 3)
eliminar_duplicados(Lista, ListaResultante) (aridad 2)
list_max(Elemento, ListaElementos) (aridad 2)
list_max2(Elemento, Elemento2, ListaElementos) (aridad 3)
my_maplist(Predicado, Lista, ListaResultante)  (aridad 3)
bubble_sortX(Pixeles, PixelesResultantes) (aridad 2)
b_sortX(Pixeles, Pixeles2, Pixeles3, PixelesOrdenados) (aridad 4)
bubbleX(Pixel1, Pixeles2, Pixeles3, PixelesOrdenados)  (aridad 4)
bubble_sortY(Pixeles, PixelesResultantes) (aridad 2)
b_sortY(Pixeles, Pixeles2, Pixeles3, PixelesOrdenados) (aridad 4)
bubbleY(Pixel1, Pixeles2, Pixeles3, PixelesOrdenados)  (aridad 4)
rAH2(Rojo, H2)  (aridad 2)
rAH1(Rojo, H1)  (aridad 2)
gAH4(Verde, H4) (aridad 2)
gAH3(Verde, H3) (aridad 2)
bAH6(Azul, H6)  (aridad 2)
bAH5(Azul, H5)  (aridad 2)
bubble_sortP(Imagenes,Imagenes2) (aridad 2)
b_sortP(Imagenes,Imagenes3,Imagenes2) (aridad 3)
bubbleP(Imagen1, Imagenes2, Imagenes3, Max) (aridad 4)
ordenarPorProfundidad(ListaImagenes, ListaImagenes2) (aridad 2)
pixbit(X, Y, BIT, D, PIXBIT) (aridad 5)
pixrgb(X, Y, R, G, B, D, PIXRGB) (aridad 7)
pixhex(X, Y, HEX, D, PIXHEX) (aridad 5)
image(Width, Height, Pixeles, IMAGE) (aridad 4)
histograma(Colores, Repeticiones, HISTOGRAM) (aridad 3)
depthLayers(Imagen, Pixeles, ListaImagenes)  (aridad 3)
getx(Pixel, X) (aridad 2)
gety(Pixel, Y) (aridad 2)
getbit(Pixbit, BIT) (aridad 2)
getd(Pixel, Profundidad) (aridad 2)
bits(Pixbits, BITS) (aridad 2)
extraerX(Pixeles, Xs) (aridad 2)
extraerY(Pixeles, Ys) (aridad 2)
getr(Pixrgb, Red)   (aridad 2)
getg(Pixrgb, Green) (aridad 2)
getb(Pixrgb, Blue)  (aridad 2)
getd_rgb(Pixrgb, Profundidad) (aridad 2)
rgbs(Pixrgbs, Colores) (aridad 2)
gethex(Pixhex, HEX) (aridad 2)
hexs(Pixhexs, Colores) (aridad 2)
getw(Imagen, Ancho) (aridad 2)
geth(Imagen, Alto)  (aridad 2)
getpixeles(Imagen, Pixeles) (aridad 2)
isPixbit(X, Y, BIT, Depth)  (aridad 4)
isPixrgb(X, Y, R, G, B, D)  (aridad 6)
numeroAletra(Numero, String) (aridad 2)
isPixhex(X, Y, HEX, D) (aridad 4)
imageIsBitmap(Imagen) (aridad 1)
imageIsPixmap(Imagen) (aridad 1)
imageIsHexmap(Imagen) (aridad 1)
imageIsCompressed(Imagen) (aridad 1)
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
filasRGB(Pixrgbs, String) (aridad 2)
todosRGB(Pixrgbs, String) (aridad 2)
cambiarPixrgbs(Pixrgbs, Pixrgbs2, Pixrgbs3) (aridad 3)
crearPixelesRGB(Pixrgbs, Pixrgbs2, Pixrgbs3) (aridad 3)
cambiarXhex(Pixhex, Largo, PixelFinal) (aridad 3)
cambiarXhexs(Pixhexs, Largo, PixelesFinales) (aridad 3)
cambiarYhex(Pixhex, Largo, PixelFinal) (aridad 3)
cambiarYhexs(Pixhexs, Largo, PixelesFinales) (aridad 3)
pixelAptoHex(Pixhex, X1, Y1, X2, Y2, PixelFinal) (aridad 6)
pixelesaptosHex(Pixhexs, X1, Y1, X2, Y2, PixelesFinales) (aridad 6)
rotateHex(Pixhex, Altura, PixelResultante) (aridad 3)
rotateHexs(Pixhexs, Altura, PixelResultante) (aridad 3)
eliminar_dePixelesHex(Pixhexs, Color, PixhexsFinales) (aridad 3)
filasHex(Pixhexs, String) (aridad 2)
todosHex(Pixhexs, String) (aridad 2)
cambiarPixhexs(Pixhexs1, Pixhexs2, Pixhexs3)  (aridad 3)
crearPixelesHexs(Pixhexs, Pixhexs2, Pixhexs3) (aridad 3)
imageFlipH(Imagen, I2) (aridad 2)
imageFlipV(Imagen, I2) (aridad 2)
imageCrop(Imagen, X1, Y1, X2, Y2, I2) (aridad 2)
imageRGBToHex(Imagen, I2) (aridad 2)
imageToHistogram(Image, Histograma) (aridad 2)
imageRotate90(Imagen, I2) (aridad 2)
imageCompress(Imagen, I2) (aridad 2)
imageChangePixel(Imagen, Pixel, I2) (aridad 3)
imageInvertColorRGB(Pixel, Pixel2)  (aridad 2)
imageToString(Image, String) (aridad 2)
imageDepthLayers(Imagen, ListaImagenes) (aridad 2)

Metas primarias




Metas secundarias
Clausulas
Hechos
Reglas
*/



%Dominio: Lista y su primer elemento.
%Descripción: Predicado que obtiene el primer elemento de una lista.
p1([Cabeza|_], Cabeza).

%Dominio: Lista y su segundo elemento.
%Descripción: Predicado que obtiene el segundo elemento de una lista.
p2([_, Cabeza2|_], Cabeza2).

%Dominio: Lista y su tercer elemento.
%Descripción: Predicado que obtiene el tercer elemento de una lista.
p3([_, _, Cabeza3|_], Cabeza3).


%Dominio: Lista y número.
%Descripción: Predicado que obtiene el largo de una lista.
my_size([], 0).
my_size([_|Y], N) :-
    my_size(Y, N1),
    N is N1 + 1.

%Dominio: Tres listas.
%Descripción: Predicado que permite agregar un elemento al final de una lista.
my_append([], L2, L2).
my_append([H|T], L2, [H|L3]) :-
    my_append(T, L2, L3).


%Dominio: Lista, elemento y número.
%Descripción: Predicado que permite obtener cuántas veces se repite un elemento en una lista.
contarRepetidos([], _, 0).
contarRepetidos([C|Q], Elemento, Numero) :-
    C = Elemento,
    contarRepetidos(Q, Elemento, N1),
    Numero is N1+1;
    contarRepetidos(Q, Elemento, Numero).

%Dominio: Tres listas.
%Descripción: Predicado que permite obtener cuántas veces se repiten los elementos de una lista en otra lista, 
%             así permitiendo crear una lista que incorpore la cantidad de veces que se repiten los elementos.
contarRepetidos2([_|_], [], []).
contarRepetidos2([C|Q], [Elemento|Elementos], N) :-
    contarRepetidos([C|Q], Elemento, N1),
    contarRepetidos2([C|Q], Elementos, N2),
    my_append([N1], N2, N).

%Dominio: Dos listas.
%Descripción: Predicado que permite eliminar todos los elementos repetidos, solo dejando uno por cada elemento.
eliminar_duplicados([], []).
eliminar_duplicados([Head|Tail], Result) :-
    member(Head, Tail), !,
    eliminar_duplicados(Tail, Result).
eliminar_duplicados([Head|Tail], [Head|Result]) :-
    eliminar_duplicados(Tail, Result).

%Dominio: Elemento de histograma e histograma.
%Descripción: Predicado que permite obtener el color que más se repite en un histograma
%             junto a la cantidad de veces que se repite.
list_max(M, [[Primer, Valor1]|Pixeles]):-
    list_max2(M, [Primer, Valor1], Pixeles).

%Dominio: Dos elementos de histograma e histogram.
%Descripción: Predicado que permite obtener el color que más se repite de un histograma
%             comparando todos los colores con sus respectivas repeticiones.
list_max2(M, M, []):- !.

list_max2([Primer, Valor1], [_, Valor2], [[Tercer, Valor3]|Pixeles2]):-
          Valor3 >= Valor2,
          !,
          list_max2([Primer, Valor1], [Tercer, Valor3], Pixeles2).

list_max2([Primer, Valor1], [Segundo, Valor2], [[_, Valor3]|Pixeles2]):-
          Valor3 =< Valor2,
          list_max2([Primer, Valor1], [Segundo, Valor2], Pixeles2).

%Dominio: Predicado y dos listas.
%Descripción: Predicado que permite aplicar un predicado a todos los elementos de una lista.
my_maplist(_, [], []).
my_maplist(P, [A|As], [B|Bs]) :-
    call(P, A, B),
    my_maplist(P, As, Bs).

%Dominio: Dos listas.
%Descripción: Predicado que obtiene los pixeles ordenados según sus valores en X.
bubble_sortX(List,Sorted):-
    b_sortX(List,[],Sorted).

%Dominio: Tres listas.
%Descripción: Predicado que permite ordenar pixeles según sus valores en X.
b_sortX([],Acc,Acc).
b_sortX([H|T],Acc,Sorted):-
    bubbleX(H,T,NT,Max),
    b_sortX(NT,[Max|Acc],Sorted).
   
%Dominio: Un pixel, dos listas y un pixel.
%Descripción: Predicado que permite ordenar pixeles con el método burbuja según sus valores en X.
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

%Dominio: Dos listas.
%Descripción: Predicado que obtiene los pixeles ordenados según sus valores en Y.
bubble_sortY(List,Sorted):-
    b_sortY(List,[],Sorted).

%Dominio: Tres listas.
%Descripción: Predicado que permite ordenar pixeles según sus valores en Y.
b_sortY([],Acc,Acc).
b_sortY([H|T],Acc,Sorted):-
    bubbleY(H,T,NT,Max),
    b_sortY(NT,[Max|Acc],Sorted).
   
%Dominio: Un pixel, dos listas y un pixel.
%Descripción: Predicado que permite ordenar pixeles con el método burbuja según sus valores en X.
bubbleY(Pixel1,[],[],Pixel1).
bubbleY(Pixel1,[Pixel2|T],[Pixel2|NT],Max):-
    gety(Pixel1, Y1),
    gety(Pixel2, Y2),
    Y1>Y2,
    bubbleY(Pixel1,T,NT,Max).
bubbleY(Pixel1,[Pixel2|T],[Pixel1|NT],Max):-
    gety(Pixel1, Y1),
    gety(Pixel2, Y2),
    Y1=<Y2,
    bubbleY(Pixel2,T,NT,Max).

%Dominio: Dos números.
%Descripción: Predicado que transformar el canal R a segundo elemento de un valor hexadecimal.
rAH2(R, H2) :-
    H2 is R rem 16.

%Dominio: Dos números.
%Descripción: Predicado que permite transformar el canal R a primer elemento de un valor hexadecimal.
rAH1(R, H1) :-
    H1 is R//16 rem 16.

%Dominio: Dos números.
%Descripción: Predicado que permite transformar el canal G a cuarto elemento de un valor hexadecimal.
gAH4(G, H2) :-
    H2 is G rem 16.

%Dominio: Dos números.
%Descripción: Predicado que permite transformar el canal G a tercer elemento de un valor hexadecimal.
gAH3(G, H1) :-
    H1 is G//16 rem 16.

%Dominio: Dos números.
%Descripción: Predicado que permite transformar el canal B a sexto elemento de un valor hexadecimal.
bAH6(B, H2) :-
    H2 is B rem 16.

%Dominio: Dos números.
%Descripción: Predicado que permite transformar el canal B a quinto elemento de un valor hexadecimal.
bAH5(B, H1) :-
    H1 is B//16 rem 16.

%Dominio: Dos listas.
%Descripción: Predicado que permite ordenar una lista de pixeles según su profundidad.
bubble_sortP(List,Sorted):-
    b_sortP(List,[],Sorted).

%Dominio: Tres listas.
%Descripción: Predicado que permite ordenar una lista de pixeles según su profundidad.
b_sortP([],Acc,Acc).
b_sortP([H|T],Acc,Sorted):-
    bubbleP(H,T,NT,Max),
    b_sortP(NT,[Max|Acc],Sorted).
   
%Dominio: Imagen, dos listas de imágenes e imagen.
%Descripción: Predicado que permite comparar los las profundidades de un conjunto de imágenes para determinar el mayor.
bubbleP(Imagen1,[],[], Imagen1).
bubbleP(Imagen1, [Imagen2|T], [Imagen2|NT], Max):-
    getpixeles(Imagen1, Pixeles1),
    getpixeles(Imagen2, Pixeles2),
    p1(Pixeles1, Pixel1),
    p1(Pixeles2, Pixel2),
    getd(Pixel1, D1),
    getd(Pixel2, D2),
    D1 > D2,
    bubbleP(Imagen1,T,NT,Max).

bubbleP(Imagen1,[Imagen2|T],[Imagen1|NT],Max):-
    getpixeles(Imagen1, Pixeles1),
    getpixeles(Imagen2, Pixeles2),
    p1(Pixeles1, Pixel1),
    p1(Pixeles2, Pixel2),
    getd(Pixel1, D1),
    getd(Pixel2, D2),
    D1 =< D2,
    bubbleP(Imagen2,T,NT,Max).

%Dominio: Dos listas de imágenes.
%Descripción: Predicado que permite ordenar una lista de imágenes según su profundidad.
ordenarPorProfundidad(ListaImagenes, ListaImagenes2) :-
    bubble_sortP(ListaImagenes, ListaImagenes2).

/*
CONSTRUCTORES
*/

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
*/
%Dominio: Cuatro números y pixbit.
%Descripción: Predicado que permite construir el TDA pixbit.
pixbit(X, Y, BIT, D, [X, Y, BIT, D]).

/*
Nombre: TDA pixrgb
Dominio:
Pixrgb: Pixrgb
- Coordenada X, Coordenada Y, Canal Red, Canal Green, Canal Blue, Profundidad: Enteros.
*/

%Dominio: Seis números y pixrgb.
%Descripción: Predicado que permite construir el TDA pixrgb.
pixrgb(X, Y, R, G, B, D, [X, Y, R, G, B, D]).

/*
Nombre: TDA pixhex
Dominio:
Pixhex: Pixhex
- Coordenada X, Coordenada Y: Enteros. 
- HexString: String.
- Profundidad: Entero.
*/
%Dominio: Cuatro números y pixhex.
%Descripción: Predicado que permite construir el TDA pixhex.
pixhex(X, Y, HEX, D, [X, Y, HEX, D]).

/*
Nombre: TDA image
Dominio: 
Image: Image
- Ancho, Alto: Enteros.
- Pixeles: Lista pixel.
*/
%Dominio: Dos números, pixeles e image.
%Descripción: Predicado que permite construir el TDA image.
image(W, H, [C|T], [W, H, Pixeles2]) :-
    number(W),
    number(H),
    ordenarPixelesX([C|T], Pixeles),
    ordenarPixelesY(Pixeles, Pixeles2).

%Dominio: Colores, repeticiones de colores e histograma.
%Descripción: Predicado que permite construir el TDA histogram.
histograma([], [], []).
histograma([C|Q], [Repeticion|Repeticiones], Result) :-
    histograma(Q, Repeticiones, Result2),
    my_append([[C, Repeticion]], Result2, Result).

%Dominio: Una imagen, pixeles y lista de imágenes.
%Descripción: 
depthLayers(_, [], []).
depthLayers(Imagen, [Pixeles1|Pixeles2], ListaImagenes) :-
    getw(Imagen, W),
    geth(Imagen, H),
    image(W, H, Pixeles1, I1),
    depthLayers(Imagen, Pixeles2, ListaImagenes2),
    my_append([I1], ListaImagenes2, ListaImagenes).

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
SELECTORES IMAGENs
*/

%Dominio: Una imagen y un número.
%Descripción: Predicado que permite obtener el ancho de una imagen.
getw([W|_], W).

%Dominio: Una imagen y un número.
%Descripción: Predicado que permite obtener el alto de una imagen.
geth([_, H|_], H).

%Dominio: Una imagen y pixeles.
%Descripción: Predicado que permite obtener los pixeles de una imagen.
getpixeles([_, _, Pixeles|_], Pixeles).

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
PERTENENCIA IMAGEN
*/
%Dominio: image.
%Descripción: Predicado que determina si una imagen es de tipo Bitmap.
imageIsBitmap(Imagen) :-
    getpixeles(Imagen, Pixeles),
    p1(Pixeles, Pixel),
    getx(Pixel, X),
    gety(Pixel, Y),
    getbit(Pixel, BIT),
    getd(Pixel, D),
    isPixbit(X, Y, BIT, D).

%Dominio: image.
%Descripción: Predicado que determina si una imagen es de tipo Pixmap.
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

%Dominio: image.
%Descripción: Predicado que determina si una imagen es de tipo Hexmap.
imageIsHexmap(Imagen) :-
    getpixeles(Imagen, Pixeles),
    p1(Pixeles, Pixel),
    getx(Pixel, X),
    gety(Pixel, Y),
    gethex(Pixel, HEX),
    getd(Pixel, D),
    isPixhex(X, Y, HEX, D).

%Dominio: image.
%Descripción: Predicado que determina si una imagen está comprimida.
imageIsCompressed(Imagen) :-
    getpixeles(Imagen, Pixeles),
    getw(Imagen, W),
    geth(Imagen, H),
    my_size(Pixeles, N),
    N =\= W*H.

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
    string_concat(H, H_tres, HEX),

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

%Dominio: Pixhexs, pixhexs y pixhexs.
%Descripción: Predicado que permite crear listas de pixhexs agrupados según su profundidad.
crearPixelesHexs([], [_|_], []).
crearPixelesHexs([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales) :-
    cambiarPixhexs([Pixel1|Pixeles1], [Pixel2|Pixeles2], PixelesFinales1),
    crearPixelesHexs(Pixeles1, [Pixel2|Pixeles2], PixelesFinales2),
    my_append([PixelesFinales1], PixelesFinales2, PixelesFinales3),
    eliminar_duplicados(PixelesFinales3, PixelesFinales).

/*
MODIFICADORES IMAGEN
*/
%Dominio: image e image.
%Descripción: Predicado que permite dar vuelta una imagen horizontalmente.
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

%Dominio: image e image.
%Descripción: Predicado que permite dar vuelta una imagen verticalmente.
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

%Dominio: image, cuatro números e image.
%Descripción: Predicado que permite recortar una imagen dados cuatro números.
imageCrop(Imagen, X1, Y1, X2, Y2, I2) :-
    getpixeles(Imagen, Pixeles),
    min_list([X1, X2], MinX1),
    min_list([Y1, Y2], MinY1),
    max_list([X1, X2], MaxX2),
    max_list([Y1, Y2], MaxY2),

    ((imageIsBitmap(Imagen),
    pixelesAptosBit(Pixeles, MinX1, MinY1, MaxX2, MaxY2, PixelesAptos1),
    exclude(integer, PixelesAptos1, PixelesAptos));

    (imageIsPixmap(Imagen),
    pixelesAptosRGB(Pixeles, MinX1, MinY1, MaxX2, MaxY2, PixelesAptos2),
    exclude(integer, PixelesAptos2, PixelesAptos));

    (imageIsHexmap(Imagen),
    pixelesAptosHex(Pixeles, MinX1, MinY1, MaxX2, MaxY2, PixelesAptos3),
    exclude(integer, PixelesAptos3, PixelesAptos))),

    extraerX(PixelesAptos, Xs),
    extraerY(PixelesAptos, Ys),
    min_list(Xs, MinX),
    min_list(Ys, MinY),
    max_list(Xs, MaxX),
    max_list(Ys, MaxY),
    Wnew is MaxX - MinX + 1,
    Hnew is MaxY - MinY + 1,
    image(Wnew, Hnew, PixelesAptos, I2).

%Dominio: image e image.
%Descripción: Predicado que permite transformar una imagen tipo Pixmap a Hexmap.
imageRGBToHex(Imagen, I2) :-
    getw(Imagen, W),
    geth(Imagen, H),
    getpixeles(Imagen, Pixeles),
    pixelesrgbApixeleshex(Pixeles, P),
    image(W, H, P, I2).

%Dominio: image e histogram.
%Descripción: Predicado que permite obtener el histograma de una imagen.
imageToHistogram(Imagen, Histogram) :-
    getpixeles(Imagen, Pixeles),
    ((imageIsBitmap(Imagen),
    bits(Pixeles, BITS),
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

%Dominio: image e image.
%Descripción: Predicado que permite rotar una imagen en 90 grados a la derecha.
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

%Dominio: image e image.
%Descripción: Predicado que permite comprimir una imagen, eliminando los pixeles con mayor ocurrencia.
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

%Dominio: image, pixel e image.
%Descripción: Predicado que permite cambiar un pixel de una imagen.
imageChangePixel(Imagen, Pixel, I2) :-
    getw(Imagen, W),
    geth(Imagen, H),
    getpixeles(Imagen, Pixeles),
    cambiarPixel(Pixeles, Pixel, Pixeles2),
    image(W, H, Pixeles2, I2).

%Dominio: Pixrgb y pixrgb.
%Descripción: Predicado que invierte los canales de un pixrgb.
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

%Dominio: image y string.
%Descripción: Predicado que permite transformar una imagen a string.
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

%Dominio: image y lista de images.
%Descripción: Predicado que permite crear una lista de imágenes agrupadas según su profundidad.
imageDepthLayers(Imagen, ListaImagenes2) :-
    getpixeles(Imagen, Pixeles),
    ((imageIsBitmap(Imagen),
    crearPixelesBits(Pixeles, Pixeles, P),
    depthLayers(Imagen, P, ListaImagenes));

    (imageIsPixmap(Imagen),
    crearPixelesRGB(Pixeles, Pixeles, P1),
    depthLayers(Imagen, P1, ListaImagenes));

    (imageIsHexmap(Imagen),
    crearPixelesHexs(Pixeles, Pixeles, P2),
    depthLayers(Imagen, P2, ListaImagenes))),
    ordenarPorProfundidad(ListaImagenes, ListaImagenes2).

