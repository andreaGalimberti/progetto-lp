%%% -*- Mode: Prolog -*-

%%% nsfa.pl starts here

is_regex([]).
is_regex([H | T]):-
    is_regex(H),
    is_regex(T).

is_regex(RE):-
    atomic(RE).

is_regex(RE):-
    compound(RE),
    RE =.. [Fun | Args],
    ( (member(Fun, [c, a]), length(Args, N), N >= 2)
    ; (member(Fun,[z,o]), length(Args,1))
    ),
    !,
    is_regex(Args).

is_regex(RE):-
    compound(RE),
    RE =.. [Functor | _],
    \+ member(Functor, [c,a,z,o]).


%%% nsfa.pl ends here


