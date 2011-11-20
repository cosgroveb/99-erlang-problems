-module(bbp).
-export([last/1]).
-export([penultimate/1]).
-export([len/1]).
-export([reverse/1]).
-export([is_palindrome/1]).
-export([flatten/1]).
-export([distinct/1]).
% -export([pack/1]).
-export([encode/1]).

%% 1.01 Find the last element of a list
last([]) -> [];
last([_,S|T]) -> last([S] ++ T);
last([Last|[]]) -> Last.

%% 1.02 Find the second to last element of a list
penultimate([]) -> "undefined";
penultimate([_|[]]) -> "undefined";
penultimate([P|[_|[]]]) -> P;
penultimate([_|[S|T]]) -> penultimate([S] ++ T).

%% 1.04 Find the length of a list
len([]) -> 0;
len(L) -> inner_len(L, 0).

inner_len([_|[]], C) -> C+1;
inner_len([_|T], C) -> inner_len(T, C+1).

%% 1.05 Reverse a list
reverse([]) -> [];
reverse([H|[]]) -> [H];
reverse([H|T]) -> reverse(T) ++ [H].

%% 1.06 Determine if a list is a palindrome
is_palindrome([]) -> true;
is_palindrome([_|[]]) -> true;

is_palindrome([Head|Tail]) ->
  case reverse(Tail) of
    [Last|Rest] when Head == Last ->
      is_palindrome(Rest);
    _ ->
     false
  end.

%% 1.07 Flatten a list of lists
flatten([]) -> [];
flatten([H|T]) when is_list(H) == false -> [H] ++ flatten(T);
flatten([H|T]) when is_list(H) -> flatten(H) ++ flatten(T).

%% 1.08 Distinct elements of a list
distinct([]) -> [];
distinct([H|[]]) -> [H];
distinct([H|[Last|[]]]) when H /= Last -> [H, Last];
distinct([H|[Last|[]]]) when H == Last -> [H];
distinct([H,S|T]) when H /= S -> [H] ++ distinct(T);
distinct([H,S|T]) when H == S -> distinct(T).

%% 1.09 Pack consecutive elements into a list of lists
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

%% 1.10 'Run-length encoding' of a list
encode([]) -> [];
encode([H|[]]) -> [[1, H]];
encode([H,S|T]) when H == S -> encode([S] ++ T, 1);
encode([H,S|T]) when H /= S -> [[1, H]] ++ encode([S] ++ T);
encode([H|[Last|[]]]) when H == Last -> [[2, H]];
encode([H|[Last|[]]]) when H /= Last -> [[1, H], [1, Last]].

encode([H|[]], Len) -> [[Len + 1, H]];
encode([H,S|T], Len) when H == S -> encode([S] ++ T, Len + 1);
encode([H,S|T], Len) when H /= S -> [[Len, H]] ++ encode([S] ++ T);
encode([H|[Last|[]]], Len) when H == Last -> [[Len + 1, Last]];
encode([H|[Last|[]]], Len) when H /= Last -> [[Len, H], [1, Last]].
