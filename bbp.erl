-module(bbp).
-export([len/1]).
-export([reverse/1]).
-export([is_palindrome/1]).
-export([flatten/1]).
-export([distinct/1]).
% -export([pack/1]).

%% 1.04 
len([]) -> 0;
len(L) -> inner_len(L, 0).

inner_len([_|[]], C) -> C+1;
inner_len([_|T], C) -> inner_len(T, C+1).

%% 1.05
reverse([]) -> [];
reverse([H|[]]) -> [H];
reverse([H|T]) -> reverse(T) ++ [H].

%% 1.06
is_palindrome([]) -> true;
is_palindrome([_|[]]) -> true;

is_palindrome([Head|Tail]) ->
  case reverse(Tail) of
    [Last|Rest] when Head == Last ->
      is_palindrome(Rest);
    _ ->
     false
  end.

%% 1.07
flatten([]) -> [];
flatten([H|T]) when is_list(H) == false -> [H] ++ flatten(T);
flatten([H|T]) when is_list(H) -> flatten(H) ++ flatten(T).

%% 1.08
distinct([]) -> [];
distinct([H|[]]) -> [H];
distinct([H|[Last|[]]]) when H /= Last -> [H, Last];
distinct([H|[Last|[]]]) when H == Last -> [H];
distinct([H,S|T]) when H /= S -> [H] ++ distinct(T);
distinct([H,S|T]) when H == S -> distinct(T).

%% 1.09
% pack(List) -> case aux_pack(List, []) of
%     {[],
%
% aux_pack([], _) -> [];
% aux_pack([H|[]], _) -> [H];
% aux_pack([H|[Last|[]]], _) when H /= Last -> [[H, Last]];
% aux_pack([H|[Last|[]]], _) when H == Last -> [[H], [Last]];
% aux_pack([Head,Second|Tail], Packed) when Head == Second ->
%   aux_pack(Tail, [Head, Second] ++ Packed);
% aux_pack([Head,Second|Tail], Packed) when Head /= Second ->
%   {[Second] ++ Tail, [Head] ++ Packed}.


% pack([]) -> [];
% pack([H|[]) -> [[H]];
% pack([H,S|[]]) when H /= S -> [[H], [S]];
% pack([H,S|[]]) when H == S -> [[H, S]];
% pack([H,S|Tail]) when H /= S -> case aux_pack([S] ++ Tail)
