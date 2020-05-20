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
-module(text).

-export([filling/2, tests/0]).

-spec filling(nonempty_string(), pos_integer()) -> string().
filling(FileName, Len) when Len > 0 ->
  Lines = get_file_contents(FileName),
  LinesWords = [[Word || Word <- string:split(Line, " ", all), length(Word) > 0] || Line <- Lines],
  Words = lists:foldr(fun erlang:'++'/2, [], LinesWords),
  horizontal_align(Words, Len).

-spec get_file_contents(nonempty_string()) -> list().
get_file_contents(Name) ->
  {ok, File} = file:open(Name, [read]),
  Rev = get_all_lines(File, []),
  lists:reverse(Rev).

-spec get_all_lines(pid(), [string()]) -> [string()].
get_all_lines(File, Partial) ->
  case io:get_line(File, "") of
    eof ->
      file:close(File),
      Partial;
    Line ->
      Strip = string:trim(Line, both, "\n"),
      get_all_lines(File, [Strip | Partial])
  end.

-spec horizontal_align([[string()]], pos_integer()) -> string().
horizontal_align(Words, LineLen) ->
  horizontal_align(left, Words, LineLen).

-spec horizontal_align(left, [string()], pos_integer()) -> string().
horizontal_align(left, Words, LineLen) ->
  lists:flatten(horizontal_align_left(Words, LineLen, 0)).

-spec horizontal_align_left([T], pos_integer(), non_neg_integer()) -> [T].
horizontal_align_left([], _LineLen, _Count) ->
  [];
horizontal_align_left([Word | Words], LineLen, Count) -> 
  NewCount = Count + length(Word),
  CanSpaceSep = NewCount + 1 =< LineLen,
  CanNewLineSep = NewCount > LineLen,
  case {CanSpaceSep, CanNewLineSep} of
    {true, false} ->
      [Word, " " | horizontal_align_left(Words, LineLen, NewCount + 1)];
    {false, true} ->
      ["\n", Word, " " | horizontal_align_left(Words, LineLen, length(Word) + 1)];
    {false, false} ->
      [Word | horizontal_align_left(Words, LineLen, NewCount)]
  end.

-spec tests() -> ok.
tests() ->
  "The heat bloomed in December as the\n"
  "carnival season kicked into gear. \n"
  "Nearly helpless with sun and glare,\n"
  "I avoided Rio's brilliant sidewalks\n"
  "and glittering beaches, panting in \n"
  "dark corners and waiting out the \n"
  "inverted southern summer. " = filling("short.txt", 35),
  ok.

