% Simple example factorial.
% Copyright (C) 2009-2010 by Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
%
% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.
-module(factorial).
-export([fac/1, fac_closed_form/1]).

-define(SQRT_5, 2.236067977499789696409173668731276235440618359611525724270897245410520925637804899414414408378782274969508176150773783504).
-define(A, (1 + ?SQRT_5) / 2).
-define(B, (1 - ?SQRT_5) / 2).

fac(0) -> 1;
fac(N) -> N * fac(N - 1).

fac_closed_form(N) -> ((1 / ?SQRT_5) * math:pow(?A, N)) - ((1 / ?SQRT_5) * math:pow(?B, N)).
