/*
X: Posición en x, entero.
Y: Posición en y, entero.
BIT: Valor del bit, entero 0 <= BIT <= 1.
D: Valor de la profundidad, entero.
*/

%CONSTRUCTORES
pixbit-d(X, Y, BIT, D, [X, Y, BIT, D]) :-
    number(X),
    number(Y),
    number(BIT),
    number(D),
    BIT is 0;
    BIT is 1.

pixrgb-d(X, Y, R, G, B, D, [X, Y, R, G, B, D]) :-
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

pixhex-d(X, Y, HEX, D, [X, Y, HEX, D]) :-
    number(X),
    number(Y),
    string(HEX),
    number(D).

image(W, H, [C|T], [W, H, [C|T]]) :-
    number(W),
    number(H).

%SELECTORES
x([X|Resto], X) :-
    number(X).

y([X, Y|Resto], Y) :-
    number(Y).

bit([X, Y, BIT|Resto], BIT) :-
    number(BIT).

r([X, Y, R|Resto], R) :-
    number(R).

g([X, Y, R, G|Resto], G) :-
    number(G).

b([X, Y, R, G, B|Resto], B) :-
    number(B).

hex([X, Y, Hex|Resto], Hex) :-
    string(Hex).

d([X, Y, Valor, D|Resto], D) :-
    number(D).

d_rgb([X, Y, R, G, B, D|Resto], D) :-
    number(D).

