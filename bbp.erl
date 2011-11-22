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
last([_,S|T]) -> last([S|T]);
last([Last|[]]) -> Last.

%% 1.02 Find the second to last element of a list
penultimate([]) -> "undefined";
penultimate([_|[]]) -> "undefined";
penultimate([P,_|[]]) -> P;
penultimate([_,S|T]) -> penultimate([S|T]).

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
flatten(List) -> case List of
  [] -> [];
  [Head|Tail] -> if
    is_list(Head) -> flatten(Head) ++ flatten(Tail);
    is_list(Head) == false -> [Head|flatten(Tail)]
  end
end.

%% 1.08 Distinct elements of a list
distinct(List) -> case List of
  [] -> [];
  [Head|[]] -> [Head];
  [Head,Second|[]] ->
    if
      Head == Second -> [Head];
      Head /= Second -> [Head, Second] 
    end;
  [Head,Second|Tail] ->
    if
      Head == Second -> distinct([Second|Tail]);
      Head /= Second -> [Head|distinct([Second|Tail])]
    end
end.

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
encode(List) -> case List of
  [] -> [];
  [H|[]] -> [[1, H]];
  [Head|[Last|[]]] ->
    if
      Head == Last -> [[2, Head]];
      Head /= Last -> [[1, Head], [1, Last]]
    end;
  [Head,Second|Tail] ->
    if
      Head == Second -> encode([Second|Tail], 1);
      Head /= Second -> [[1, Head]|encode([Second|Tail])]
    end
end.

encode([H,S|T], L) ->
  if H == S -> encode([S] ++ T, L + 1);
     true -> [[L + 1, H]] ++ encode([S] ++ T)
end.
