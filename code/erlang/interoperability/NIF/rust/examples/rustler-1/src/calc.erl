-module(calc).

-include("crates.hrl").

-export([sum/2, subtract/2, multiplies/2, divides/2]).

-on_load(init/0).

init() ->
    ok = ?load_nif_from_crate(calc, ?crate_calc, 0).

sum(_A, _B) -> exit(nif_library_not_loaded).
subtract(_A, _B) -> exit(nif_library_not_loaded).
multiplies(_A, _B) -> exit(nif_library_not_loaded).
divides(_A, _B) -> exit(nif_library_not_loaded).