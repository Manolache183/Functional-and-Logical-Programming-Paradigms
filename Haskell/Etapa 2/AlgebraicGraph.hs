module AlgebraicGraph where

import qualified Data.Set as S

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)
    deriving (Eq, Show)

-- (1, 2), (1, 3)
angle :: AlgebraicGraph Int
angle = Connect (Node 1) (Overlay (Node 2) (Node 3))

-- (1, 2), (1, 3), (2, 3)
triangle :: AlgebraicGraph Int
triangle = Connect (Node 1) (Connect (Node 2) (Node 3))

{-
    *** TODO ***

    Mulțimea nodurilor grafului.

    Hint: S.union
-}
nodes :: Ord a => AlgebraicGraph a -> S.Set a
nodes Empty = S.empty -- niciun nod
nodes (Node val) = S.singleton val -- nodul dat
nodes (Overlay g1 g2) = S.union (nodes g1) (nodes g2) -- suma nodurilor din ambele grafuri
nodes (Connect g1 g2) = S.union (nodes g1) (nodes g2)

{-
    *** TODO ***

    Mulțimea arcelor grafului.

    Hint: S.union, S.cartesianProduct

-}
edges :: Ord a => AlgebraicGraph a -> S.Set (a, a)
edges Empty = S.empty -- niciun arc intr-un graf gol
edges (Node val) = S.empty -- niciun arc intr-un graf cu un singur nod
edges (Overlay g1 g2) = S.union (edges g1) (edges g2) -- overlay doar reuneste 2 grafuri
edges (Connect g1 g2) = S.union (S.union (edges g1) (edges g2)) (S.cartesianProduct (nodes g1) (nodes g2)) -- overlay + noile arce

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
outNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
outNeighbors node Empty = S.empty -- niciun arc de la nodul meu la un graf gol
outNeighbors node (Node val) = S.empty -- niciun arc de la nodul meu la un graf cu un nod
outNeighbors node (Overlay g1 g2) = S.union (outNeighbors node g1) (outNeighbors node g2) -- nu am arce de la g1 la g2 deci caut individual
outNeighbors node (Connect g1 g2) = S.union (S.union (outNeighbors node g1) (outNeighbors node g2)) ( -- la fel ca mai sus doar ca adaug
                        if (S.member node (nodes g1)) then (nodes g2) -- nodurile din graful 2, daca nodul meu e continut in primul graf
                        else S.empty)

{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
-- La fel ca mai sus doar ca invers la connect
inNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
inNeighbors node Empty = S.empty
inNeighbors node (Node val) = S.empty
inNeighbors node (Overlay g1 g2) = S.union (inNeighbors node g1) (inNeighbors node g2)
inNeighbors node (Connect g1 g2) = S.union (S.union (inNeighbors node g1) (inNeighbors node g2)) (
                        if (S.member node (nodes g2)) then (nodes g1)
                        else S.empty)

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, se întoarce același graf.

    Hint: Definiți o funcție recursivă locală (de exemplu, în where),
    care să primească drept parametri doar entități variabile de la un apel
    recursiv la altul, astfel încât să nu copiați la fiecare apel parametrii
    nemodificați. De exemplu, parametrul node nu se modifică, în timp ce
    parametrul graph se modifică.
-}
removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
removeNode node Empty = Empty
removeNode node (Node val) = if node == val then Empty else (Node val) -- caut nodul si l scot practic inainte de constructie
removeNode node (Overlay g1 g2) = Overlay (removeNode node g1) (removeNode node g2)
removeNode node (Connect g1 g2) = Connect (removeNode node g1) (removeNode node g2)

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.
    
    Hint: Funcție recursivă locală, ca la removeNode.
-}
splitNode :: Eq a
          => a                 -- nodul divizat
          -> [a]               -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
splitNode old news Empty = Empty
splitNode old news (Node val) = if (old == val) then f news else (Node val) -- aici e schema, creez un graf cu toate nodurile
                        where
                            f :: Eq a => [a] -> AlgebraicGraph a
                            f vals = if (length vals == 0) then Empty 
                                     else if (length vals == 1) then (Node (head vals))
                                     else Overlay (Node (head vals)) (f (tail vals))
splitNode old news (Overlay g1 g2) = Overlay (splitNode old news g1) (splitNode old news g2)
splitNode old news (Connect g1 g2) = Connect (splitNode old news g1) (splitNode old news g2) 

{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Hint: Funcție recursivă locală, ca la removeNode.
-}
mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut
mergeNodes prop node Empty = Empty
mergeNodes prop node (Node val) = if (prop val) then (Node node) else (Node val) -- efectiv inlocuiesc nodul
mergeNodes prop node (Overlay g1 g2) = Overlay (mergeNodes prop node g1) (mergeNodes prop node g2)
mergeNodes prop node (Connect g1 g2) = Connect (mergeNodes prop node g1) (mergeNodes prop node g2) 
