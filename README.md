# Mini Caml

# Descrizione: Progettazione e sviluppo di un interprete in OCaml

Il progetto prevede la progettazione e realizzazione di una semplice estensione del linguaggio didattico
funzionale presentato a lezione che permetta di manipolare _insiemi_. Un insieme
è una collezione di valori, non ordinati, che non contiene valori duplicati.

Un insieme può essere creato a partire dall'insieme vuoto

`let s = empty(t)`

(**risultato è l'insieme vuoto di tipo t**)

dove t è il tipo degli elementi dell'insieme.


Alternativamente può essere creato attraverso un valore:

`let s = singleton("hello", String)`

(**risultato è un insieme di stringhe contenente la string "hello"**)


Infine può essere creato a partire da espresioni



`let s = of(int, 3+5, succ(5))`

(**risultato è l'insieme di interi che contiene i valori 7 e 6**)


Gli insiemi sono caratterizzati da numerose operazioni di base (tra cui **unione**, **intersezione**, **differenza**).
Ipotozziamo inoltre di avere altre operazioni che permettono:
1. inserire (rimuovere) un elemento da un insieme,
2. controllare se un insieme è vuoto,
3. controllare se un elemento appartiene ad un insieme,
4. controllare il minimo (massimo) valore di un insieme,

Oltre a questo insieme di operazioni di base, il tipo insieme prevede un insieme di 
operatori di natura "funzionale".

- `For_all(predicate, aSet)` controlla se tutti gli elementi dell'insieme soddisfano 
la proprietà definita dal parametro "predicate". Il parametro "predicate" è una funzione
che applicata ad un elemento dell'insieme restituisce un valore booleano.
- `Exists(predicate, aSet)` controlla se esiste almeno un elemento dell'insieme che
soddisfa la proprietà definita dal parametro "predicate".
- `Filter(predicate, aSet)` restituisce l'insieme degli elementi dell'insieme che soddisfano la proprietà definita dal parametro "predicate".
- `Map(function, aSet)` restituisce l'insieme dei valori v tali che `v = function(x)` dove `x` appartiene a `aSet`.


1. Definire le regole operazionali per l'introduzione del tipo di dato nel linguaggio didattico.
2. Definire le regole operazionali per le classi di operazione previste dal tipo di dato set
3. Estendere l'interprete OCaml del linguaggio funzionale assumendo scoping statico.
4. Definire il type checker dinamico del linguaggio risultante.
5. Si verifichi la correttezza dell'interprete progettando ed eseguendo una quantità di casi di test
sufficiente a testare tutti gli operatori aggiuntivi.
6. Opzionale: definire il type checker statico.
