:-include(tdapixrgb_21305689_CamposOlguin).
:-include(tdapixbit_21305689_CamposOlguin).
:-include(tdapixhex_21305689_CamposOlguin).
/*
CONSTRUCTORES
*/

/*
Nombre: TDA image
Dominio: 
Image: Image
- Ancho, Alto: Enteros.
- Pixeles: Lista pixel.

Predicados:
image(Width, Height, Pixeles, IMAGE) (aridad 4)
getw(Imagen, Ancho) (aridad 2)
geth(Imagen, Alto)  (aridad 2)
getpixeles(Imagen, Pixeles) (aridad 2)
imageIsBitmap(Imagen) (aridad 1)
imageIsPixmap(Imagen) (aridad 1)
imageIsHexmap(Imagen) (aridad 1)
imageIsCompressed(Imagen) (aridad 1)
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
bubble_sortX(Pixeles, PixelesResultantes) (aridad 2)
b_sortX(Pixeles, Pixeles2, Pixeles3, PixelesOrdenados) (aridad 4)
bubbleX(Pixel1, Pixeles2, Pixeles3, PixelesOrdenados)  (aridad 4)
bubble_sortY(Pixeles, PixelesResultantes) (aridad 2)
b_sortY(Pixeles, Pixeles2, Pixeles3, PixelesOrdenados) (aridad 4)
bubbleY(Pixel1, Pixeles2, Pixeles3, PixelesOrdenados)  (aridad 4)

Metas primarias: getw, geth, getpixeles, imageIsBitmap, imageIsPixmap,
                 imageIsHexmap, imageIsCompressed, imageFlipH, imageFlipV, 
                 imageCrop, imageRGBToHex, imageToHistogram, imageRotate90, 
                 imageCompress, imageChangePixel, imageInvertColorRGB, 
                 imageToString e imageDepthLayers.
Metas secundarias: bubble_sortX, b_sortX, bubbleX, bubble_sortY, b_sortY y bubbleY. 

Clausulas
- Reglas:

%Dominio: Dos números, pixeles e image.
%Descripción: Predicado que permite construir el TDA image.
*/
image(W, H, [C|T], [W, H, Pixeles2]) :-
    number(W),
    number(H),
    ordenarPixelesX([C|T], Pixeles),
    ordenarPixelesY(Pixeles, Pixeles2).

/*
Nombre: TDA histogram
Dominio: 
Histogram: Histogram
- Colores: [R, G, B] | [0|1] | String
- Repeticiones: Entero >= 0.

Predicados:
histograma(Colores, Repeticiones, HISTOGRAM) (aridad 3)
my_append(Lista, Lista2, ListaResultante)  (aridad 3)
contarRepetidos(Lista, Elemento, Cantidad) (aridad 3)
contarRepetidos2(Lista, ListaElementos, ListaResultante) (aridad 3)
eliminar_duplicados(Lista, ListaResultante) (aridad 2)
list_max(Elemento, ListaElementos) (aridad 2)
list_max2(Elemento, Elemento2, ListaElementos) (aridad 3)

Metas primarias: contarRepetidos, contarRepetidos2 y eliminar_duplicados.
Metas secundarias: my_append, list_max y list_max2.

Clausulas
- Reglas:

%Dominio: Colores, repeticiones de colores e histograma.
%Descripción: Predicado que permite construir el TDA histogram.
*/
histograma([], [], []).
histograma([C|Q], [Repeticion|Repeticiones], Result) :-
    histograma(Q, Repeticiones, Result2),
    my_append([[C, Repeticion]], Result2, Result).

/*
Nombre: TDA DepthLayers
Dominio:
DepthLayers: DepthLayers
- Imagen: Image
- Pixeles: Pixeles

Predicados:
depthLayers(Imagen, Pixeles, ListaImagenes)  (aridad 3)
bubble_sortP(Imagenes,Imagenes2) (aridad 2)
b_sortP(Imagenes,Imagenes3,Imagenes2) (aridad 3)
bubbleP(Imagen1, Imagenes2, Imagenes3, Max) (aridad 4)
ordenarPorProfundidad(ListaImagenes, ListaImagenes2) (aridad 2)

Metas primarias: ordenarPorProfundidad.
Metas secundarias: bubble_sortP, b_sortP y bubbleP.

Clausulas
- Reglas:

%Dominio: Una imagen, pixeles y lista de imágenes.
%Descripción: Predicado que permite construir una lista de imágenes cambiando
el color de los pixeles que no sean de la misma profundidad.
*/
depthLayers(_, [], []).
depthLayers(Imagen, [Pixeles1|Pixeles2], ListaImagenes) :-
    getw(Imagen, W),
    geth(Imagen, H),
    image(W, H, Pixeles1, I1),
    depthLayers(Imagen, Pixeles2, ListaImagenes2),
    my_append([I1], ListaImagenes2, ListaImagenes).

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
    hexs(Pixeles, HEXS),
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
    Rnew is 255 - R,
    Gnew is 255 - G,
    Bnew is 255 - B,
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

/*
OTROS PREDICADOS
p1(Lista, PrimerElemento)  (aridad 2)
p2(Lista, SegundoElemento) (aridad 2)
p3(Lista, TercerElemento)  (aridad 2)
my_size(Lista, Largo) (aridad 2)
my_maplist(Predicado, Lista, ListaResultante)  (aridad 3)
rAH2(Rojo, H2)  (aridad 2)
rAH1(Rojo, H1)  (aridad 2)
gAH4(Verde, H4) (aridad 2)
gAH3(Verde, H3) (aridad 2)
bAH6(Azul, H6)  (aridad 2)
bAH5(Azul, H5)  (aridad 2)
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