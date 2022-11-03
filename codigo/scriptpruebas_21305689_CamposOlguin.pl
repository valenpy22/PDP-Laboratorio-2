
%SCRIPT PRUEBAS PROVEÍDAS POR EL PROFESOR VÍCTOR FLORES.

pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB), pixbit( 1, 0, 0, 30, PC), pixbit( 1, 1, 1, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsBitmap(I), imageToString(I, Str),write(Str).
pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB), pixbit( 1, 0, 0, 30, PC), pixbit( 1, 1, 1, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsBitmap( I ).
pixhex( 0, 0, "#FF0000", 10, PA), pixhex( 0, 1, "#FF0000", 20, PB), pixhex( 1, 0, "#0000FF", 30, PC), pixhex( 1, 1, "#0000FF", 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsBitmap( I ).
pixrgb( 0, 0, 200, 200, 200, 10, PA), pixrgb( 0, 1, 200, 200, 200, 20, PB), pixrgb( 1, 0, 190, 190, 190, 30, PC), pixrgb( 1, 1, 190, 190, 190, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsBitmap( I ).
pixhex( 0, 0, "#FF0000", 10, PA), pixhex( 0, 1, "#FF0000", 20, PB), pixhex( 1, 0, "#0000FF", 30, PC), pixhex( 1, 1, "#0000FF", 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageToString(I, Str),write(Str).
pixhex( 0, 0, "#FF0000", 10, PA), pixhex( 0, 1, "#FF0000", 20, PB), pixhex( 1, 0, "#0000FF", 30, PC), pixhex( 1, 1, "#0000FF", 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsHexmap( I ).
pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB), pixbit( 1, 0, 0, 30, PC), pixbit( 1, 1, 1, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsHexmap( I ).
pixrgb( 0, 0, 200, 200, 200, 10, PA), pixrgb( 0, 1, 200, 200, 200, 20, PB), pixrgb( 1, 0, 190, 190, 190, 30, PC), pixrgb( 1, 1, 190, 190, 190, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsHexmap( I ).
pixrgb( 0, 0, 255, 0, 0, 10, PA), pixrgb( 0, 1, 255, 0, 0, 20, PB), pixrgb( 1, 0, 0, 0, 255, 30, PC), pixrgb( 1, 1, 0, 0, 255, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageToString(I, Str),write(Str).
pixrgb( 0, 0, 200, 200, 200, 10, PA), pixrgb( 0, 1, 200, 200, 200, 20, PB), pixrgb( 1, 0, 190, 190,190, 30, PC), pixrgb( 1, 1, 190, 190, 190, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsPixmap( I ).
pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB), pixbit( 1, 0, 0, 30, PC), pixbit( 1, 1, 1, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsPixmap( I ).
pixhex( 0, 0, "#FF0000", 10, PA), pixhex( 0, 1, "#FF0000", 20, PB), pixhex( 1, 0, "#0000FF", 30, PC), pixhex( 1, 1, "#0000FF", 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsPixmap( I ).
pixrgb( 0, 0, 200, 200, 200, 10, PA), pixrgb( 0, 1, 200, 200, 200, 20, PB), pixrgb( 1, 0, 190, 190,190, 30, PC), pixrgb( 1, 1, 190, 190, 190, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsPixmap( I ), imageRGBToHex(I, I2), imageIsHexmap(I2), imageToString(I2, Str), write(Str).
pixhex( 0, 0, "#FF0000", 10, PA), pixhex( 0, 1, "#FF0000", 20, PB), pixhex( 1, 0, "#0000FF", 30, PC), pixhex( 1, 1, "#0000FF", 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageRotate90(I, I2), imageRotate90(I2, I3), imageRotate90(I3, I4), imageRotate90(I4, I5).
pixhex( 0, 0, "#FF0000", 30, PA), pixhex( 0, 1, "#FF0000", 30, PB), pixhex( 1, 0, "#FF0000", 30, PC), pixhex( 1, 1, "#FF0000", 30, PD), image( 2, 2, [PA, PB, PC, PD], I), imageRotate90(I, I2).
pixhex( 0, 0, "#FF0000", 10, PA), pixhex( 0, 1, "#FF0000", 20, PB), pixhex( 1, 0, "#0000FF", 30, PC), pixhex( 1, 1, "#0000FF", 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageFlipV(I, I2), imageFlipV(I2, I3).
pixhex( 0, 0, "#FF0000", 10, PA), pixhex( 0, 1, "#FF0000", 20, PB), pixhex( 1, 0, "#0000FF", 30, PC), pixhex( 1, 1, "#0000FF", 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageFlipH(I, I2), imageFlipH(I2, I3).
pixhex( 0, 0, "#FF0000", 30, PA), pixhex( 0, 1, "#FF0000", 30, PB), pixhex( 1, 0, "#FF0000", 30, PC), pixhex( 1, 1, "#FF0000", 30, PD), image( 2, 2, [PA, PB, PC, PD], I), imageFlipH(I, I2).
pixhex( 0, 0, "#FF0000", 20, PA), pixhex( 0, 1, "#FF0000", 20, PB), pixhex( 0, 2, "#FF0000", 20, PC), pixhex( 1, 0, "#0000FF", 30, PD), pixhex( 1, 1, "#0000FF", 4, PE), pixhex( 1, 2, "#0000FF", 4, PF), pixhex( 2, 0, "#0000FF", 4, PG), pixhex( 2, 1, "#0000FF", 4, PH), pixhex( 2, 2, "#0000FF", 4, PI), image( 3, 3, [PA, PB, PC, PD, PE, PF, PG, PH, PI], I), imageCrop( I, 1, 1, 2, 2, I2), pixhex( 0, 0, "#0000FF", 4, PE2), pixhex( 0, 1, "#0000FF", 4, PF2), pixhex( 1, 0, "#0000FF", 4, PH2), pixhex( 1, 1, "#0000FF", 4, PI2), image( 2, 2, [PE2, PF2, PH2, PI2], I3).
pixrgb( 0, 0, 10, 10, 10, 10, P1), pixrgb( 0, 1, 20, 20, 20, 20, P2), pixrgb( 1, 0, 30, 30, 30, 30, P3), pixrgb( 1, 1, 40, 40, 40, 40, P4), image( 2, 2, [P1, P2, P3, P4], I1), pixrgb( 0, 1, 54, 54, 54, 20, P2_modificado), imageChangePixel(I1, P2_modificado, I2).
pixrgb( 0, 0, 33, 33, 33, 10, PA), pixrgb( 0, 1, 44, 44, 44, 10, PB), pixrgb( 1, 0, 55, 55, 55, 30, PC), pixrgb( 1, 1, 66, 66, 66, 30, PD), image( 2, 2, [PA, PB, PC, PD], I), imageDepthLayers(I, [PRIMERA, SEGUNDA]), pixrgb( 0, 0, 33, 33, 33, 10, PA2), pixrgb( 0, 1, 44, 44, 44, 10, PB2), pixrgb( 1, 0, 255, 255, 255, 10, PC2), pixrgb( 1, 1, 255, 255, 255, 10, PD2), image( 2, 2, [PA2, PB2, PC2, PD2], I2), pixrgb( 0, 0, 255, 255, 255, 30, PA3), pixrgb( 0, 1, 255, 255, 255, 30, PB3), pixrgb( 1, 0, 55, 55, 55, 30, PC3), pixrgb( 1, 1, 66, 66, 66, 30, PD3), image( 2, 2, [PA3, PB3, PC3, PD3], I3).

%No implementado el decompress
/*Comprime una imagen, luego descomprime y debe resultar la misma imagen original:
pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB), pixbit( 1, 0, 0, 30, PC), pixbit( 1, 1, 1, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageCompress(I, I2), imageDecompress(I2, I3).*/

%SCRIPT PRUEBAS PROVEÍDAS POR MI PERSONA.
pixbit(0, 0, 0, 1, P1), pixbit(0, 1, 0, 3, P2), pixbit(1, 0, 1, 5, P3), pixbit(1, 1, 0, 4, P4), image(2, 2, [P1, P2, P3, P4], I), imageToString(I, Str), write(Str).
pixrgb(0, 0, 255, 255, 255, 1, P1), pixrgb(0, 1, 12, 14, 12, 4, P2), image(1, 2, [P1, P2], I), imageToString(I, Str), write(Str).
pixhex(0, 0, "#00FF00", 6, P1), pixhex(1, 0, "#000000", 14, P2), pixhex(2, 0, "#FF00FF", 20, P3), image(3, 1, [P1, P2, P3], I), imageToString(I, Str), write(Str).

pixrgb(0, 0, 200, 150, 200, 10, PA), pixrgb(0, 1, 200, 4, 2, 20, PB), pixrgb(1, 0, 1, 5, 6, 30, PC), pixrgb(1, 1, 7, 9, 0, 4, PD), image(2, 2, [PA, PB, PC, PD], I), imageIsBitmap(I).
pixbit(0, 0, 0, 1, P1), pixbit(0, 1, 0, 3, P2), pixbit(1, 0, 1, 5, P3), pixbit(1, 1, 0, 4, P4), image(2, 2, [P1, P2, P3, P4], I), imageIsHexmap(I).
pixhex(0, 0, "#00FF00", 6, P1), pixhex(1, 0, "#000000", 14, P2), pixhex(2, 0, "#FF00FF", 20, P3), image(3, 1, [P1, P2, P3], I), imageIsPixmap(I).

pixrgb(0, 0, 200, 150, 200, 10, PA), pixrgb(0, 1, 200, 4, 2, 20, PB), pixrgb(1, 0, 1, 5, 6, 30, PC), pixrgb(1, 1, 7, 9, 0, 4, PD), image(2, 2, [PA, PB, PC, PD], I), imageIsCompressed(I).
pixbit(0, 0, 0, 1, P1), pixbit(0, 1, 0, 3, P2), pixbit(1, 0, 1, 5, P3), pixbit(1, 1, 0, 4, P4), image(2, 2, [P1, P2, P3, P4], I), imageIsCompressed(I).
pixhex(0, 0, "#00FF00", 6, P1), pixhex(1, 0, "#000000", 14, P2), pixhex(2, 0, "#FF00FF", 20, P3), image(3, 1, [P1, P2, P3], I), imageIsCompressed(I).

pixrgb(0, 0, 200, 150, 200, 10, PA), pixrgb(0, 1, 200, 4, 2, 20, PB), pixrgb(1, 0, 1, 5, 6, 30, PC), pixrgb(1, 1, 7, 9, 0, 4, PD), image(2, 2, [PA, PB, PC, PD], I), imageFlipH(I, I2).
pixbit(0, 0, 0, 1, P1), pixbit(0, 1, 0, 3, P2), pixbit(1, 0, 1, 5, P3), pixbit(1, 1, 0, 4, P4), image(2, 2, [P1, P2, P3, P4], I), imageFlipH(I, I2).
pixhex(0, 0, "#00FF00", 6, P1), pixhex(1, 0, "#000000", 14, P2), pixhex(2, 0, "#FF00FF", 20, P3), image(3, 1, [P1, P2, P3], I), imageFlipH(I, I2).

pixrgb(0, 0, 200, 150, 200, 10, PA), pixrgb(0, 1, 200, 4, 2, 20, PB), pixrgb(1, 0, 1, 5, 6, 30, PC), pixrgb(1, 1, 7, 9, 0, 4, PD), image(2, 2, [PA, PB, PC, PD], I), imageFlipV(I, I2).
pixbit(0, 0, 0, 1, P1), pixbit(0, 1, 0, 3, P2), pixbit(1, 0, 1, 5, P3), pixbit(1, 1, 0, 4, P4), image(2, 2, [P1, P2, P3, P4], I), imageFlipV(I, I2).
pixhex(0, 0, "#00FF00", 6, P1), pixhex(1, 0, "#000000", 14, P2), pixhex(2, 0, "#FF00FF", 20, P3), image(3, 1, [P1, P2, P3], I), imageFlipV(I, I2).

pixbit(0, 0, 0, 1, P1), pixbit(0, 1, 0, 3, P2), pixbit(1, 0, 1, 5, P3), pixbit(1, 1, 0, 4, P4), image(2, 2, [P1, P2, P3, P4], I), imageCrop(I, 0, 0, 0, 0, I2).
pixrgb(0, 0, 200, 150, 200, 10, PA), pixrgb(0, 1, 200, 4, 2, 20, PB), pixrgb(1, 0, 1, 5, 6, 30, PC), pixrgb(1, 1, 7, 9, 0, 4, PD), image(2, 2, [PA, PB, PC, PD], I), imageCrop(I, 0, 0, 0, 1, I2).
pixhex(0, 0, "#00FF00", 6, P1), pixhex(1, 0, "#000000", 14, P2), pixhex(2, 0, "#FF00FF", 20, P3), image(3, 1, [P1, P2, P3], I), imageCrop(I, 1, 0, 2, 0, I2).

pixrgb(0, 0, 200, 150, 200, 10, PA), pixrgb(0, 1, 200, 4, 2, 20, PB), pixrgb(1, 0, 1, 5, 6, 30, PC), pixrgb(1, 1, 7, 9, 0, 4, PD), image(2, 2, [PA, PB, PC, PD], I), imageRGBToHex(I, I2).
pixrgb(0, 0, 6, 6, 6, 10, PA), pixrgb(0, 1, 3, 4, 50, 20, PB), pixrgb(1, 0, 3, 100, 54, 30, PC), pixrgb(1, 1, 4, 12, 12, 4, PD), image(2, 2, [PA, PB, PC, PD], I), imageRGBToHex(I, I2).
pixrgb(0, 0, 54, 12, 142, 112, PA), pixrgb(0, 1, 123, 4, 123, 20, PB), pixrgb(1, 0, 255, 100, 54, 30, PC), pixrgb(1, 1, 12, 12, 12, 4, PD), image(2, 2, [PA, PB, PC, PD], I), imageRGBToHex(I, I2).

pixbit(0, 0, 0, 1, P1), pixbit(0, 1, 0, 3, P2), pixbit(1, 0, 1, 5, P3), pixbit(1, 1, 0, 4, P4), image(2, 2, [P1, P2, P3, P4], I), imageToHistogram(I, H).
pixrgb(0, 0, 255, 255, 255, 1, P1), pixrgb(0, 1, 12, 14, 12, 4, P2), image(1, 2, [P1, P2], I), imageToHistogram(I, H).
pixhex(0, 0, "#00FF00", 6, P1), pixhex(1, 0, "#000000", 14, P2), pixhex(2, 0, "#FF00FF", 20, P3), image(3, 1, [P1, P2, P3], I), imageToHistogram(I, H).

pixrgb(0, 0, 200, 150, 200, 10, PA), pixrgb(0, 1, 200, 4, 2, 20, PB), pixrgb(1, 0, 1, 5, 6, 30, PC), pixrgb(1, 1, 7, 9, 0, 4, PD), image(2, 2, [PA, PB, PC, PD], I), imageRotate90(I, I2).
pixbit(0, 0, 0, 1, P1), pixbit(0, 1, 0, 3, P2), pixbit(1, 0, 1, 5, P3), pixbit(1, 1, 0, 4, P4), image(2, 2, [P1, P2, P3, P4], I), imageRotate90(I, I2).
pixhex(0, 0, "#00FF00", 6, P1), pixhex(1, 0, "#000000", 14, P2), pixhex(2, 0, "#FF00FF", 20, P3), image(3, 1, [P1, P2, P3], I), imageRotate90(I, I2).

pixrgb(0, 0, 200, 150, 200, 10, PA), pixrgb(0, 1, 200, 4, 2, 20, PB), pixrgb(1, 0, 1, 5, 6, 30, PC), pixrgb(1, 1, 7, 9, 0, 4, PD), image(2, 2, [PA, PB, PC, PD], I), imageCompress(I, I2).
pixbit(0, 0, 0, 1, P1), pixbit(0, 1, 0, 3, P2), pixbit(1, 0, 1, 5, P3), pixbit(1, 1, 0, 4, P4), image(2, 2, [P1, P2, P3, P4], I), imageCompress(I, I2).
pixhex(0, 0, "#00FF00", 6, P1), pixhex(1, 0, "#000000", 14, P2), pixhex(2, 0, "#FF00FF", 20, P3), image(3, 1, [P1, P2, P3], I), imageCompress(I, I2).

pixrgb(0, 0, 200, 150, 200, 10, PA), pixrgb(0, 1, 200, 4, 2, 20, PB), pixrgb(1, 0, 1, 5, 6, 30, PC), pixrgb(1, 1, 7, 9, 0, 4, PD), image(2, 2, [PA, PB, PC, PD], I), pixrgb(0, 1, 0, 0, 0, 0, PA), imageChangePixel(I, PA, I2).
pixbit(0, 0, 0, 1, P1), pixbit(0, 1, 0, 3, P2), pixbit(1, 0, 1, 5, P3), pixbit(1, 1, 0, 4, P4), image(2, 2, [P1, P2, P3, P4], I), pixbit(0, 1, 1, 123, PA), imageChangePixel(I, PA, I2).
pixhex(0, 0, "#00FF00", 6, P1), pixhex(1, 0, "#000000", 14, P2), pixhex(2, 0, "#FF00FF", 20, P3), image(3, 1, [P1, P2, P3], I), pixhex(2, 0, "#BCEDAF", 12, PA), imageChangePixel(I, PA, I2).

pixrgb(0, 0, 10, 10, 10, 10, P1), pixrgb(0, 1, 20, 20, 20, 20, P2), pixrgb(1, 0, 30, 30, 30, 30, P3), pixrgb(1, 1, 40, 40, 40, 40, P4), image(2, 2, [P1, P2, P3, P4], I1), imageInvertColorRGB(P2, P2_modificado), imageChangePixel(I1, P2_modificado, I2).
pixrgb(0, 0, 200, 150, 200, 10, PA), pixrgb(0, 1, 200, 4, 2, 20, PB), pixrgb(1, 0, 1, 5, 6, 30, PC), pixrgb(1, 1, 7, 9, 0, 4, PD), image(2, 2, [PA, PB, PC, PD], I), imageInvertColorRGB(P2, P2_modificado), imageChangePixel(I, P2_modificado, I2).
pixrgb(0, 0, 5, 200, 5, 10, PA), pixrgb(0, 1, 200, 5, 200, 20, PB), pixrgb(1, 0, 5, 190, 5, 30, PC), pixrgb(1, 1, 190, 5, 190, 4, PD), image(2, 2, [PA, PB, PC, PD], I), imageInvertColorRGB(P2, P2_modificado), imageChangePixel(I, P2_modificado, I2).

pixrgb(0, 0, 200, 150, 200, 10, PA), pixrgb(0, 1, 200, 4, 2, 20, PB), pixrgb(1, 0, 1, 5, 6, 30, PC), pixrgb(1, 1, 7, 9, 0, 4, PD), image(2, 2, [PA, PB, PC, PD], I), imageDepthLayers(I, Lista).
pixbit(0, 0, 0, 1, P1), pixbit(0, 1, 0, 3, P2), pixbit(1, 0, 1, 5, P3), pixbit(1, 1, 0, 4, P4), image(2, 2, [P1, P2, P3, P4], I), imageDepthLayers(I, Lista).
pixhex(0, 0, "#00FF00", 6, P1), pixhex(1, 0, "#000000", 14, P2), pixhex(2, 0, "#FF00FF", 20, P3), image(3, 1, [P1, P2, P3], I), imageDepthLayers(I, Lista).