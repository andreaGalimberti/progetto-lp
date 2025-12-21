% -----------------------------------------------------------
% 1. DEFINIZIONE DELLA WHITELIST (I tuoi funtori permessi)
% Qui elenchi SOLO i funtori che accetti come nodi validi.
% -----------------------------------------------------------
funtore_permesso(seq).   % Sequenza
funtore_permesso(star).  % Stella di Kleene
funtore_permesso(or).    % Scelta alternativa
funtore_permesso(plus).  % Esempio: uno o più
% funtore_permesso(c).   % Se vuoi usare 'c' come nel tuo esempio precedente

% -----------------------------------------------------------
% 2. CASO LISTA VUOTA (Stop)
% -----------------------------------------------------------
is_regex([]) :- !.

% -----------------------------------------------------------
% 3. CASO LISTA NON VUOTA (Iterazione argomenti)
% -----------------------------------------------------------
is_regex([Testa | Coda]) :- 
    !, 
    is_regex(Testa), 
    is_regex(Coda).

% -----------------------------------------------------------
% 4. CASO ATOMO/NUMERO (Foglia)
% -----------------------------------------------------------
is_regex(Termine) :-
    atomic(Termine), 
    !.

% -----------------------------------------------------------
% 5. CASO COMPOSTO (Nodo dell'albero) - MODIFICATO
% -----------------------------------------------------------
is_regex(Termine) :-
    compound(Termine),
    
    % A. Controllo validità: Il funtore DEVE esistere nella whitelist
    functor(Termine, Funtore, _),
    funtore_permesso(Funtore),  % <--- QUI: Verifica positiva (senza \+)
    
    % B. Estrazione Argomenti
    Termine =.. [_ | Argomenti], 
    
    % C. Ricorsione sui figli
    is_regex(Argomenti).