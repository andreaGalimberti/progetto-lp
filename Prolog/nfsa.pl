%%% -*- Mode: Prolog -*-

%%% nfsa.pl

%!   is_regex/1
%
%   True when RE is a valid regular expression.
%
%   A regular expression is consider valid when: 
%   - is a atoms/numbers or compounds except for c,a,z,o,
%   - 

is_regex(RE):-
    atomic(RE).

is_regex(RE):-
    compound(RE),
    RE =.. [Fun | Args],
    ( (member(Fun, [c, a]), length(Args, N), N >= 2)
    ; (member(Fun,[z,o]), length(Args,1))
    ),
    !,
    is_regex_check_args(Args).

is_regex(RE):-
    compound(RE),
    RE =.. [Functor | _],
    \+ member(Functor, [c,a,z,o]).


%!   is_regex_check_args/1
%
%   True when all elements in the list are valid regular expressions.
%   Helper predicate for recursive check of arguments.

is_regex_check_args([]).
is_regex_check_args([H | T]):-
    is_regex(H),
    is_regex_check_args(T).




%%% ---------------------------------------------------------------------------
%%% SECTION 2: COMPILER (Regex -> NFSA)
%%% ---------------------------------------------------------------------------

% Dichiarazione dei predicati dinamici che compongono la base dati dell'automa.
% 
:- dynamic nfsa_init/2.
:- dynamic nfsa_final/2.
:- dynamic nfsa_delta/4.

%!  nfsa_comp_regex/2
%
%   Compiles a Regex (RE) into an NFSA identified by FA_Id.
%   Based on Thompson's Construction Algorithm.
%   

nfsa_comp_regex(FA_Id, RE) :-
    % 1. Verifica che sia una regex valida (usa il predicato fatto al punto 1)
    is_regex(RE),
    % 2. Pulisce eventuali definizioni precedenti per questo ID (buona pratica)
    nfsa_clear(FA_Id),
    % 3. Genera Stato Iniziale e Finale univoci per l'intero automa
    gensym(q, InitialState),
    gensym(q, FinalState),
    % 4. Asserisce i punti di ingresso e uscita dell'automa 
    assert(nfsa_init(FA_Id, InitialState)),
    assert(nfsa_final(FA_Id, FinalState)),
    % 5. Avvia la compilazione ricorsiva
    compile_step(RE, FA_Id, InitialState, FinalState).


% Helper per pulire l'automa (anticipazione parziale del punto 4 per robustezza)
nfsa_clear(FA_Id) :-
    retractall(nfsa_init(FA_Id, _)),
    retractall(nfsa_final(FA_Id, _)),
    retractall(nfsa_delta(FA_Id, _, _, _)).

%%% ---------------------------------------------------------------------------
%%% INTERNAL COMPILATION LOGIC (Thompson's Construction)
%%% ---------------------------------------------------------------------------

% CASO 1: SEQUENZA - c(R1, R2, ...)
% Collega gli elementi in serie.
compile_step(RE, Id, Start, End) :-
    compound(RE),
    RE =.. [c | Args],
    !, % Cut per confermare che è una sequenza
    compile_seq(Args, Id, Start, End).

% CASO 2: ALTERNATIVA - a(R1, R2, ...)
% Crea una biforcazione (branching) per ogni opzione.
compile_step(RE, Id, Start, End) :-
    compound(RE),
    RE =.. [a | Args],
    !,
    compile_alt(Args, Id, Start, End).

% CASO 3: KLEENE STAR - z(R)
% 0 o più volte. Include loop e skip.
compile_step(RE, Id, Start, End) :-
    compound(RE),
    RE =.. [z, Arg],
    !,
    gensym(q, Pre),
    gensym(q, Post),
    % Percorso 0 volte: Start -> epsilon -> End
    assert(nfsa_delta(Id, Start, epsilon, End)),
    % Ingresso nel loop: Start -> epsilon -> Pre
    assert(nfsa_delta(Id, Start, epsilon, Pre)),
    % Compilazione interna: Pre -> ... -> Post
    compile_step(Arg, Id, Pre, Post),
    % Loop back: Post -> epsilon -> Pre
    assert(nfsa_delta(Id, Post, epsilon, Pre)),
    % Uscita dal loop: Post -> epsilon -> End
    assert(nfsa_delta(Id, Post, epsilon, End)).

% CASO 4: ONE OR MORE - o(R)
% 1 o più volte. Simile a Star ma senza lo skip iniziale (0 volte).
compile_step(RE, Id, Start, End) :-
    compound(RE),
    RE =.. [o, Arg],
    !,
    gensym(q, Pre),
    gensym(q, Post),
    % Ingresso obbligatorio: Start -> epsilon -> Pre
    assert(nfsa_delta(Id, Start, epsilon, Pre)),
    % Compilazione interna
    compile_step(Arg, Id, Pre, Post),
    % Loop back: Post -> epsilon -> Pre
    assert(nfsa_delta(Id, Post, epsilon, Pre)),
    % Uscita: Post -> epsilon -> End
    assert(nfsa_delta(Id, Post, epsilon, End)).

% CASO 5: SIMBOLO (Atomico o Composto non riservato)
% Crea una transizione diretta.
compile_step(Symbol, Id, Start, End) :-
    % Se siamo arrivati qui, non è c, a, z, o grazie ai cut sopra.
    % Asserisce la transizione diretta per il simbolo.
    assert(nfsa_delta(Id, Start, Symbol, End)).


%%% ---------------------------------------------------------------------------
%%% HELPERS FOR LISTS (Sequences and Alternatives)
%%% ---------------------------------------------------------------------------

% compile_seq(List, Id, Start, End)
% Caso base: un solo elemento, lo colleghiamo direttamente a End.
compile_seq([Last], Id, Start, End) :-
    !,
    compile_step(Last, Id, Start, End).

% Caso ricorsivo: Elemento -> (NuovoStato) -> Resto
compile_seq([H | T], Id, Start, End) :-
    gensym(q, MidPoint),
    compile_step(H, Id, Start, MidPoint),
    compile_seq(T, Id, MidPoint, End).


% compile_alt(List, Id, Start, End)
% Compila ogni alternativa in parallelo tra Start e End.
compile_alt([], _, _, _).
compile_alt([H | T], Id, Start, End) :-
    % Per isolare i loop, Thompson standard usa stati intermedi e epsilon.
    % Start -> epsilon -> SubStart -> [H] -> SubEnd -> epsilon -> End
    gensym(q, SubStart),
    gensym(q, SubEnd),
    assert(nfsa_delta(Id, Start, epsilon, SubStart)),
    compile_step(H, Id, SubStart, SubEnd),
    assert(nfsa_delta(Id, SubEnd, epsilon, End)),
    compile_alt(T, Id, Start, End).




%%% nsfa.pl ends here



