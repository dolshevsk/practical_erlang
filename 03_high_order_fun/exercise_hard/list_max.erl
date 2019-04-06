%% Реализовать функцию, которая принимает список, и возвращает максимальный элемент этого списка
-module(list_max).

-export([max/1]).

max(List) ->
[H|T] = List,
lists:foldl(fun(A,B) when A>=B-> A; (_,B) ->B end, H,T).