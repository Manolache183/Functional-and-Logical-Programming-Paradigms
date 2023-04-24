:- dynamic detailed_mode_disabled/0.
:- ensure_loaded('files.pl').
% student file for Ultimate Tic Tac Toe implementation

% initialState/1
% initialState(-State)
% Este adevărat pentru starea inițială a jocului.
initialState(([X, X, X,
              X, X, X,
              X, X, X], '')) :- empty_board(X), !.
initialState(_) :- false.

% getBoards/2
% getBoards(+State, -Boards)
% Este adevărat dacă în starea State, informațiile din tablele individuale sunt
% cele din variabila Boards.
% Boards este legată la o listă de 9 elemente, fiecare element reprezentând o tablă.
% Ordinea tablelor este cea din lista positions (din utils.pl).
% Fiecare element din listă este o listă de 9 elemente, reprezentând
% pozițiile de pe tablă, ca x, 0, sau ''.
% Pozițiile sunt în ordinea din lista positions (din utils.pl).

getBoards(([], _), []) :- true.
%getBoards(([H1|T1],_), [H2|T2]) :- H1 = H2, getBoards((T1, _), T2).
getBoards((L1, _), L2) :- L1 = L2.

% getBoard/3
% getBoard(+State, +UPos, -Board)
% Este adebărat dacă în starea State, la poziția UPos din tabla de UTTT, 
% se află tabla individuală cu reprezentarea din Board.
% Reprezentarea tablei este descrisă în predicatul getBoards/2.

% Cautam indexul coresp lui Upos, apoi vedem daca tabla de la Idx din State este chiar Board
getBoard((StateBoard, _), Upos, Board) :- positions(L), nth0(Idx, L, Upos), 
                                nth0(Idx, StateBoard, Board).

% getUBoard/2
% getUBoard(stare(+Board, +UboardState, +Player, +NextMoves),
% -UboardState)
% Întoarce reprezentarea UBoard-ului, indicând tablele individuale câștigate,
% remizate, sau încă în desfășurare. Reprezentarea este aceeași ca a tablelor
% individuale (vezi getBoards/2).

% getUBoard (9 Boards, Uboard), rez pe fiecare celula in parte

getUBoard(([], _), []).
getUBoard(([H1|T1], _), [H2|T2]) :- getBoardResult(H1, H2), getUBoard((T1, _), T2).

% getPos/4
% getPos(+State, +UPos, +Pos, -Cell).
% Este adevărat dacă în starea State, în tabla individuală de la poziția UPos în UBoard,
% la poziția Pos pe tablă, se află simbolul Cell (x, 0, sau '').

% Cautam tabla Board la pozitia Upos din State, apoi cautam Cell la Pos in Board
getPos((StateBoard, _), Upos, Pos, Cell) :- getPos(StateBoard, Upos, Board), getPos(Board, Pos, Cell).

% getPos/3
% getPos(+Board, +Pos, -Cell).
% Este adevărat dacă în tabla individuală reprezentată în Board, la poziția Pos, 
% se află simbolul Cell (x, 0, sau ''). Predicatul poate fi folosit și pentru UBoard, caz 
% în care Cell poate fi și r.

% Luam indexul si cautam acolo Cell in Board
getPos(Board, Pos, Cell) :- positions(L), nth0(Idx, L, Pos),
                            nth0(Idx, Board, Cell).

% getNextPlayer/2
% getNextPlayer(+State), -NextPlayer)
% Este adevărat dacă în starea State, jucătorul care urmează este NextPlayer
% (poate fi x sau 0)..

countOccurences(_, [], 0).
countOccurences(P, [H|T], Acc) :-
        findall(P, member(P, H), Bag), % Bag = atati de x cati apar in P
        length(Bag, Len), % len de Bag
        countOccurences(P, T, Y),
        Acc is Y + Len.

%numaram cati de x si 0 avem, nr egal muta x, alfel muta 0
getNextPlayer((StateBoard, _), x) :-
        countOccurences(x, StateBoard, Xs), countOccurences(0, StateBoard, Zeros), Xs =:= Zeros, !.
getNextPlayer(_, 0).

% getNextAvailableBoards/2
% getNextAvailableBoards(+State, -NextBoardsPoss)
% Este adevărat dacă în starea State, pozițiile din NextBoardsPoss sunt pozițiile 
% din UBoard ale tablelor disponibile pentru următoarea mutare.

getNextAvailableBoards((StateBoard, P), NextBoardsPoss) :-
        getUBoard((StateBoard, _), UBoard), % gasim UBoard
        positions(AllPositions),
        nth0(Idx, AllPositions, P), % cautam indexul lui P in positions
        nth0(Idx, UBoard, ''), % daca la acel index se mai joaca
        NextBoardsPoss = [P], !. % returneaza [P]

getNextAvailableBoards((StateBoard, _), NextBoardsPoss) :-
        getUBoard((StateBoard, _), UBoard), % gasim UBoard
        positions(AllPositions),
        findall(Idx, nth0(Idx, UBoard, ''), Bag), % toti indexii din Uboard care au ''
        findall(Pos, (member(Idx, Bag), nth0(Idx, AllPositions, Pos)), NextBoardsPoss). % toate Pos de la indexii din positions

% getBoardResult/2
% getBoardResult(+Board, -Result)
% Este adevărat dacă pentru o tablă individuală (sau UBoard) cu reprezentarea
% din Board, rezultatul este Result. Result poate fi:
% x sau 0, dacă jucătorul respectiv a câștigat jocul pe tabla dată;
% r, dacă s-a ajuns la remiză (toate pozițiile au fost completate dar
% tabla nu a fost câștigată);
% '', dacă tabla nu a fost câștigată și nu s-au completat toate pozițiile.
% NOTĂ: este deja definit predicatul player_wins/2 în utils.pl.
getBoardResult(Board, Result) :-
    ((player_wins(P, Board), Result = P); % tabla a fost castigata de playerul P
    (\+ nth0(_, Board, ''), Result = r)), !. % avem remiza (nu exista niciun element gol)

getBoardResult(_, ''). % nu e gata jocul

% buildState/3
% buildState(+Boards, +PreviousPos, -State)
% Este adevărat dacă starea State corespunde stării jocului în care tablele
% individuale sunt cele din lista Boards, iar ultima mutare a fost în 
% poziția PreviousPos într-o tablă individuală.
% NOTĂ: nu contează în care tablă individuală s-a realizat ultima mutare.

% verificam daca macar o pozitie de la indexul Idx are x sau 0 in oricare dintre cele 9 table
checkNonEmptyPos(_, []) :- false.
checkNonEmptyPos(Idx, [H | T]) :- nth0(Idx, H, x); nth0(Idx, H, 0); checkNonEmptyPos(Idx, T).

% echivalente Boards cu State si trebuie gasit indexul potrivit pentru PreviousPos si apoi trimis la functia de mai sus
buildState(Boards, PreviousPos, (StateBoard, PreviousPos)) :- 
                                getBoards((StateBoard, PreviousPos), Boards), 
                                positions(L), nth0(Idx, L, PreviousPos),
                                checkNonEmptyPos(Idx, Boards).

% validMove/2
% validMove(+State, +Move)
% Este adevărat dacă mutarea Move este legală în starea State.
% Move este fie o poziție, în cazul în care este o singură tablă disponibilă
% pentru a următoarea mutare din starea State, fie o pereche de poziții, altfel.

finishedGame(Board) :-
    \+ nth0(_, Board, ''); % nicio pozitie libera
    player_wins(x, Board); % a castigat x
    player_wins(0, Board). % a castigat 0

head([H|_], H). % primul element din lista       

validMove((StateBoard, PreviousPos), Pos) :-
        getUBoard((StateBoard, PreviousPos), UBoard),
        \+ finishedGame(UBoard), % nu s a terminat meciul in tabla mare
        getNextAvailableBoards((StateBoard, PreviousPos), NextAvailableBoards), % lista de next
        length(NextAvailableBoards, Len),
        Len = 1, % o singura mutare disp
        head(NextAvailableBoards, UPos), % elementul din lista
        getBoard((StateBoard, _), UPos, Board), % tabla in care se face mutarea
        positions(AllPositions), % toate pozitiile dintr o tabla
        nth0(Idx, AllPositions, Pos), % indexul din toate pozitiile al mutarii Pos
        nth0(Idx, Board, ''), !. % la acel index avem spatiu sa mutam

validMove((StateBoard, PreviousPos), (UPos, Pos)) :-
        getUBoard((StateBoard, PreviousPos), UBoard),
        \+ finishedGame(UBoard), % nu s a terminat meciul in tabla mare
        getBoard((StateBoard, _), UPos, Board), % tabla in care se face mutarea
        \+ finishedGame(Board), % nu s-a terminat jocul in tabla aleasa
        positions(AllPositions), % toate pozitiile dintr o tabla
        nth0(Idx, AllPositions, Pos), % indexul din toate pozitiile al mutarii Pos
        nth0(Idx, Board, ''). % la acel index avem spatiu sa mutam

% makeMove/3
% makeMove(+State, +Move, -NewState)
% Este adevărat dacă în urma aplicării mutării Move în starea State
% rezulta starea NewState.
% Move este fie o poziție (din lista positions), în cazul în care nu sunt mai 
% multe table disponibile pentru a următoarea mutare din starea State,
% fie o pereche de poziții, altfel.
%
% Hint: folosiți validMove pentru a verifica mutarea și buildState pentru a construi o stare.

% functia asta e luata fix de pe pagina cu nth0/4
replace_nth0(List, Index, OldElem, NewElem, NewList) :-
   % predicate works forward: Index,List -> OldElem, Transfer
   nth0(Index,List,OldElem,Transfer),
   % predicate works backwards: Index,NewElem,Transfer -> NewList
   nth0(Index,NewList,NewElem,Transfer).

makeMove((StateBoard, PreviousPos), Pos, (NewStateBoard, Pos)) :-
        validMove((StateBoard, PreviousPos), Pos), % verificam mutarea tinand cont de vechiul state
        getNextPlayer((StateBoard, _), Player), % vedem urmatorul player
        getBoard((StateBoard, _), PreviousPos, Board), % Board-ul in care se face mutarea
        positions(AllPositions),
        nth0(Idx1, AllPositions, Pos), % indicele coresp mutarii
        replace_nth0(Board, Idx1, _, Player, NewBoard), % inlocuim elementul in Board
        nth0(Idx2, AllPositions, PreviousPos), % indicele corespunzator Board-ului in tabla mare
        replace_nth0(StateBoard, Idx2, _, NewBoard, NewStateBoard), % inlocuim NewBoard in UBoard
        buildState(NewStateBoard, Pos, (NewStateBoard, Pos)), !. % Construim noul State, unde PrevPos e Move

makeMove((StateBoard, PreviousPos), (UPos, Pos), (NewStateBoard, Pos)) :-
        validMove((StateBoard, PreviousPos), (UPos, Pos)), % verificam mutarea tinand cont de vechiul state
        getNextPlayer((StateBoard, _), Player), % vedem urmatorul player
        getBoard((StateBoard, _), UPos, Board), % Board-ul in care se face mutarea
        positions(AllPositions),
        nth0(Idx1, AllPositions, Pos), % indicele coresp mutarii
        replace_nth0(Board, Idx1, _, Player, NewBoard), % inlocuim elementul in Board
        nth0(Idx2, AllPositions, UPos), % indicele corespunzator Board-ului in tabla mare
        replace_nth0(StateBoard, Idx2, _, NewBoard, NewStateBoard), % inlocuim NewBoard in UBoard
        buildState(NewStateBoard, Pos, (NewStateBoard, Pos)). % Construim noul State, unde PrevPos e Move

% dummy_first/2
% dummy_first(+State, -NextMove)
% Predicatul leagă NextMove la următoarea mutare pentru starea State.
% Strategia este foarte simplă: va fi aleasă cea mai din stânga-sus mutare posibilă
% (prima din lista de poziții disponibile).
dummy_first((StateBoard, PreviousPos), NextMove) :-
        findall(Pos, validMove((StateBoard, PreviousPos), Pos), Bag),
        head(Bag, NextMove).

% dummy_last/2
% dummy_last(+State, -NextMove)
% Predicatul leagă NextMove la următoarea mutare pentru starea State.
% Strategia este foarte simplă: va fi aleasă cea mai din dreapta-jos mutare posibilă 
% (ultima din lista de poziții disponibile).

% last(+List, -Element)
last([], _).
last([X], X).
last([_|T], X) :- last(T, X).

dummy_last((StateBoard, PreviousPos), NextMove) :-
        findall(Pos, validMove((StateBoard, PreviousPos), Pos), Bag),
        last(Bag, NextMove).


% ======== Etapa 2

% movePriority/4
% movePriority(+Player, +Board, +Move, -Priority)
% Calculează prioritatea mutării Move pentru jucătorul Player, într-o
% tablă individuală Board. Vezi enunț.

% Mutarea intr-o tabla individuala
makeMoveIndividualBoard(Player, Board, Move, NewBoard) :-
        positions(AllPositions),
        nth0(Idx, AllPositions, Move),
        replace_nth0(Board, Idx, _, Player, NewBoard).

% Mutare valida intr-o tabla individuala
validMoveIndividualBoard(Board, Move) :-
        positions(AllPositions),
        nth0(Idx, AllPositions, Move),
        nth0(Idx, Board, '').

% Mutarile in colturi
corner(nw).
corner(ne).
corner(sw).
corner(se).

% Castiga Player
movePriority(Player, Board, Move, 0) :-
        validMoveIndividualBoard(Board, Move),
        makeMoveIndividualBoard(Player, Board, Move, NewBoard),
        player_wins(Player, NewBoard), !.

% Castiga NextPlayer
movePriority(Player, Board, Move, 1) :-
        validMoveIndividualBoard(Board, Move),
        nextPlayer(Player, NextPlayer),
        movePriority(NextPlayer, Board, Move, 0), !.

% Mutare in colt
movePriority(_, Board, Move, 2) :-
        validMoveIndividualBoard(Board, Move),
        empty_board(Board),
        corner(Move), !.

% Mutare in colt
movePriority(Player, Board, Move, 3) :-
        validMoveIndividualBoard(Board, Move),
        \+ nth0(_, Board, Player), % inca nu a mutat in aceasta tabla
        nextPlayer(Player, NextPlayer), 
        nth0(4, Board, NextPlayer), % urmatorul a mutat in centru deja
        corner(Move), !. % mutare in colt

% Mutare in centru
movePriority(Player, Board, c, 3) :-
        validMoveIndividualBoard(Board, c),
        \+ nth0(_, Board, Player), !. % inca nu a mutat in aceasta tabla

% Mutare ce preceda o mutare castigatoare
movePriority(Player, Board, Move, 4) :-
        validMoveIndividualBoard(Board, Move),
        makeMoveIndividualBoard(Player, Board, Move, NewBoard), % facem mutarea
        movePriority(Player, NewBoard, _, 0), !. % putem face o mutare castigatoare

movePriority(_, Board, Move, 5) :-
        validMoveIndividualBoard(Board, Move),
        corner(Move), !.

% Sa se poata face mutarea
movePriority(_, Board, Move, 6) :-
        validMoveIndividualBoard(Board, Move).

% bestIndividualMoves/3
% bestIndividualMoves(+P, +Board, -Moves)
% Leagă Moves la o listă cu toate mutările disponibile, în ordinea
% priorității lor.
%
% Hint: construiți o listă de perechi (prioritate, mutare) și folosiți
% sortMoves/2 pentru a obține lista de mutări, în ordinea priorității.
bestIndividualMoves(Player, Board, Moves) :-
        positions(AllPositions),
        findall(Move, (member(Move, AllPositions), validMoveIndividualBoard(Board, Move)), Bag1), % toate mutarile disp
        findall((Priority, Move), (member(Move, Bag1), movePriority(Player, Board, Move, Priority)), Bag2), % mutarile cu prioritati
        sortMoves(Bag2, Moves). % sortare


% narrowGreedy/2
% narrowGreedy(+State, -Move)
% Strategie care întotdeauna ia cea mai bună mutare individuală.
% Dacă sunt mai multe table disponibile, ia tabla care este cea mai bună
% mutare individuală în raport cu U-board.

% Mai multe mutari disp
narrowGreedy(State, (UMove, Move)) :-
        getNextAvailableBoards(State, NextBoardsPoss),
        length(NextBoardsPoss, Len),
        Len > 1, % mai multe table disponibile
        getUBoard(State, UBoard), % luam tabla finala
        getNextPlayer(State, Player), 
        bestIndividualMoves(Player, UBoard, UMoves), % cele mai bune mutari in tabla mare
        head(UMoves, UMove),
        getBoard(State, UMove, Board), % tabla mica de la UMove
        bestIndividualMoves(Player, Board, Moves),
        head(Moves, Move), !.

% O mutare disp
narrowGreedy((StateBoard, PreviousPos), Move) :-
        getBoard((StateBoard, PreviousPos), PreviousPos, Board),
        getNextPlayer((StateBoard, PreviousPos), Player),
        bestIndividualMoves(Player, Board, Moves),
        head(Moves, Move), !.


% bestMoves/2
% bestMoves(+State, -Moves)
% Leagă Moves la o listă care conține toate mutările disponibile, în
% ordinea priorității lor, după ordonarea prezentată în enunț.
bestMoves(_, _) :- false.
        

% greedy/2
% greedy(+State, -Move)
% Strategie care alege cea mai bună mutare, bazat pe rezultatul lui
% bestMoves/2.
greedy(_, _) :- false.
