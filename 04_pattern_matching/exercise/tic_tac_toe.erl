-module(tic_tac_toe).

-export([new_game/0, win/1, move/3]).


new_game() ->
    {{f, f, f},
     {f, f, f},
     {f, f, f}}.


win(GameState) ->
    case GameState of
        {{Y, Y, Y},
        {_, _, _},
        {_, _, _}} when Y =/= f -> {win,Y};
        
        {{Y, _, _},
        {Y, _, _},
        {Y, _, _}} when Y =/= f -> {win,Y};

        {{_, _, _},
        {Y, Y, Y},
        {_, _, _}} when Y =/= f -> {win,Y};

        {{_, _, _},
        {_, _, _},
        {Y, Y, Y}} when Y =/= f -> {win,Y};

        {{_, Y, _},
        {_, Y, _},
        {_, Y, _}} when Y =/= f -> {win,Y};

        {{_, _, Y},
        {_, _, Y},
        {_, _, Y}} when Y =/= f -> {win,Y};

        {{Y, _, _},
        {_, Y, _},
        {_, _, Y}} when Y =/= f -> {win,Y};

        {{_, _, Y},
        {_, Y, _},
        {Y, _, _}} when Y =/= f -> {win,Y};

        _ -> no_win
        end.



move(Cell, Player, GameState) ->
        case {Cell, GameState} of
            {1, {{f,Y2,Y3},V2,V3}} when Player == x orelse Player == o -> {ok, {{Player,Y2,Y3},V2,V3}};
            {2, {{Y1,f,Y3},V2,V3}} when Player == x orelse Player == o -> {ok, {{Y1,Player,Y3},V2,V3}};
            {3, {{Y1,Y2,f},V2,V3}} when Player == x orelse Player == o -> {ok, {{Y1,Y2,Player},V2,V3}};
            {4, {V1,{f,Y2,Y3},V3}} when Player == x orelse Player == o -> {ok, {V1,{Player,Y2,Y3},V3}};
            {5, {V1,{Y1,f,Y3},V3}} when Player == x orelse Player == o -> {ok, {V1,{Y1,Player,Y3},V3}};
            {6, {V1,{Y1,Y2,f},V3}} when Player == x orelse Player == o -> {ok, {V1,{Y1,Y2,Player},V3}};
            {7, {V1,V2,{f,Y2,Y3}}} when Player == x orelse Player == o -> {ok, {V1,V2,{Player,Y2,Y3}}};
            {8, {V1,V2,{Y1,f,Y3}}} when Player == x orelse Player == o -> {ok, {V1,V2,{Y1,Player,Y3}}};
            {9, {V1,V2,{Y1,Y2,f}}} when Player == x orelse Player == o -> {ok, {V1,V2,{Y1,Y2,Player}}};
            _ -> {error,invalid_move}
        end.
