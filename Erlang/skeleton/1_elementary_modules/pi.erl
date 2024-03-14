-module (pi).
-export ([pi/0]).

% ============================ %
%  Calls the main pi function. %
% ============================ %

pi() -> calc(1,3,-1).

% =================================================== %
% A - current value in brackets (1 - 1/3 + 1/5 - ...)
% B - denominator
% C - sign(+/-)

% Recursive function that calculates pi.
% =================================================== %
calc(A, B, C) when abs(C/B) < 0.00001 -> A*4;
calc(A, B, C) -> calc(A + C/B, B + 2, -1 * C).