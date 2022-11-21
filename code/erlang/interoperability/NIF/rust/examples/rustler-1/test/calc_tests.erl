-module(calc_tests).

-include_lib("eunit/include/eunit.hrl").

it_works_test() ->
    ?assertEqual(2, calc:sum(1, 1)),
    ?assertEqual(2, calc:subtract(3, 1)),
    ?assertEqual(2, calc:multiplies(2, 1)),
    ?assertEqual(2, calc:divides(10, 5)).
