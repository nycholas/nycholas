% Copyright (c) 2020, Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
% All rights reserved.
%
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are met:
%
% % Redistributions of source code must retain the above copyright notice,
%    this list of conditions and the following disclaimer.
% % Redistributions in binary form must reproduce the above copyright notice,
%    this list of conditions and the following disclaimer in the documentation
%    and/or other materials provided with the distribution.
% % Neither the name of the Nycholas de Oliveira e Oliveira nor the names of
%    its contributors may be used to endorse or promote products derived from
%    this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.
-module(bill).

-export([printer_v1/1, printer_v2/1, tests/0]).

-spec printer_v1([pos_integer()]) -> string().
printer_v1(Barcodes) ->
    printer(get_products_by(Barcodes)).

-spec printer_v2([pos_integer()]) -> string().
printer_v2(Barcodes) ->
    Products = get_products_by(Barcodes),
    DiscountProducts = make_products_discount(Products),
    printer(Products ++ DiscountProducts).

-spec printer([pos_integer()]) -> string().
printer(Products) ->
    PHeader = lists:flatten(string:pad("Erlang Stores", 30, both)),
    PProducts = products_printer(Products, 0),
    lists:flatten(PHeader ++ "\n\n" ++ PProducts).

-spec products_printer([string()], integer()) -> [string()].
products_printer([], Total) ->
    Name = "Total",
    PTotal = io_lib:format("~.2f", [Total / 100]),
    PadLen = 30 - string:length(PTotal),
    "\n" ++ string:pad(Name, PadLen, trailing, $.) ++ PTotal;
products_printer([{_Id, Name, Value} | Products], Total) ->
    PValue = io_lib:format("~.2f", [Value / 100]),
    PadLen = 30 - string:length(PValue),
    Line = string:pad(Name, PadLen, trailing, $.) ++ PValue ++ "\n",
    [Line | products_printer(Products, Total + Value)].

-spec make_products_discount([{pos_integer(), string(), integer()}]) -> [{pos_integer(), string(), integer()}].
make_products_discount(Products) ->
    Discounts = length([1 || {Id, _, _} <- Products, Id == 1234]) div 2,
    [{1234, "Dry Sherry, 1lt (-)", -100} || _N <- lists:seq(1, Discounts)].

-spec get_products_by([pos_integer()]) -> [{pos_integer(), string(), integer()}].
get_products_by([]) ->
    [];
get_products_by([Barcode | Barcodes]) ->
    [db_product_find_by_id(Barcode) | get_products_by(Barcodes)].

-spec tests() -> ok.
tests() ->
    "        Erlang Stores         \n\n"
    "Dry Sherry, 1lt...........5.40\n"
    "Fish Fingers..............1.21\n"
    "Orange Jelly..............0.56\n"
    "Hula Hoops (Giant)........1.33\n"
    "Unknown Item..............0.00\n"
    "Dry Sherry, 1lt...........5.40\n\n"
    "Total....................13.90" = printer_v1([1234, 4719, 3814, 1112, 1113, 1234]),

    "        Erlang Stores         \n\n"
    "Dry Sherry, 1lt...........5.40\n"
    "Fish Fingers..............1.21\n"
    "Orange Jelly..............0.56\n"
    "Hula Hoops (Giant)........1.33\n"
    "Unknown Item..............0.00\n"
    "Dry Sherry, 1lt...........5.40\n"
    "Dry Sherry, 1lt (-)......-1.00\n\n"
    "Total....................12.90" = printer_v2([1234, 4719, 3814, 1112, 1113, 1234]),

    ok.

-spec db_product_findall() -> [{pos_integer(), string(), integer()}].
db_product_findall() -> 
    [
        {4719, "Fish Fingers", 121},
        {5643, "Nappies", 1010},
        {3814, "Orange Jelly", 56},
        {1111, "Hula Hoops", 21},
        {1112, "Hula Hoops (Giant)", 133},
        {1234, "Dry Sherry, 1lt", 540}
    ].

-spec db_product_find_by_id(pos_integer()) -> {pos_integer(), string(), integer()}.
db_product_find_by_id(Id) ->
    case lists:keyfind(Id, 1, db_product_findall()) of
        false ->
            {Id, "Unknown Item", 0};
        P -> 
            P
    end.