%% Реализовать Fizz Buzz
%% https://habr.com/ru/post/298134/

-module (fizz_buzz).

-export ([fizz_buzz/1]).

fizz_buzz(List) ->
    lists:foldr(fun(X,Acc) when X rem 3==0, X rem 5==0, X =/=0 -> ["FizzBuzz"|Acc]; (X,Acc) when X rem 5==0, X =/=0 -> ["Buzz"|Acc]; (X,Acc) when X rem 3==0, X =/=0 -> ["Fizz"|Acc]; (X,Acc)->[X|Acc] end, [], List).
