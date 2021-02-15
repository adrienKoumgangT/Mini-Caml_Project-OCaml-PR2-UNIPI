
(* Realizzazione di una semplice estensione 
	del linguaggio didatico funzionale presentato a lezione 
	che permetta di manipolare insiemi. *)

type ide = string;;

type exp = EInt of int
         | EFloat of float
         | EString of string
         | ETrue
         | EFalse
         | Den of ide
         | Sum of exp * exp
         | Sub of exp * exp
         | Times of exp * exp
         | Div of exp * exp
         | Mod of exp * exp
         | Concat of exp * exp
         | Maj of exp * exp
         | Inf of exp * exp
         | Ifthenelse of exp * exp * exp
         | Eq of exp * exp
         | Or of exp * exp
         | And of exp * exp
         | Not of exp
         | Let of ide * exp * exp
         | Fun of ide * exp
         | Letrec of ide * ide * exp * exp
         | Apply of exp * exp 
         | EmptySet of ide (* Definizione del costruttore di insieme vuoto *)
         | Singleton of exp * ide (* Definizione del costruttore di insieme con un singolo elemento *)
         | Of of ide * (exp list) (* Definizione del costruttore di insieme con una lista di elementi *)
         | Union of exp * exp (* Definizione dell'operatore di unione fra 2 insiemi *)
         | Inter of exp * exp (* Definizione dell'operatore di intersezione fra 2 insiemi *)
         | Diff of exp * exp (* Definizione dell'operatore di differenza fra 2 insiemei *)
         | Insert of exp * exp (* Definizione dell'operazione di inserimento di un elemento in un insieme *)
         | RemoveSet of exp * exp (* Definizione dell'operazione di cancellazione di un elemento in un insieme *)
         | IsEmpty of exp (* Definizione dell'operazione di verifica di insieme vuoto *)
         | HasElement of exp * exp (* Definizione dell'operazione di verifica di appartenenza di un elemento ad un insieme *)
         | IsSubSet of exp * exp (* Definizione dell'operazione di verifica di sottoinsieme *)
         | MaxSet of exp (* Definizione dell'operazione di ricerca del massimo valore di un insieme *)
         | MinSet of exp (* Definizione dell'operazione di ricerca del minimo valore di un insieme *)
         | ForAll of exp * exp (* Definizione dell'operazione di controllo su tutti gli elementi di un insieme *)
         | Exists of exp * exp (* Definizione dell'operazione di controllo su tutti gli elementi di un insieme *)
         | Filter of exp * exp (* Definizione dell'operazione di fitragio di elementi di un insieme *)
         | Map of exp * exp (* Definizione dell'operzione di applicazione di una funzione a degli elementi di un insieme *)
;;

type 'v env = (string * 'v) list;;

type evT = Int of int 
         | Float of float
         | Bool of bool 
         | String of string
         | SetT of ide * (evT list) (* Introduzione del tipo insiemistico *)
         | Closure of ide * exp * evT env 
         | RecClosure of ide * ide * exp * evT env 
         | Unbound
;;

let emptyEnv  = [ ("", Unbound)] ;;

let bind (s: evT env) (i:string) (x:evT) = ( i, x ) :: s;;

let rec lookup (s:evT env) (i:string) = match s with
  | [] ->  Unbound
  | (j,v)::sl when j = i -> v
  | _::sl -> lookup sl i
;;

(* funzione che mi permette di induire il tipo di un certo dato *)
let typeof x = match x with
  | Int(_) -> "int"
  | Float(_) -> "float"
  | Bool(_) -> "bool"
  | String(_) -> "string"
  | SetT(_,_) -> "set"
  | Closure(_,_,_) -> "closure"
  | RecClosure(_,_,_,_) -> "recclosure"
  | Unbound -> "unbound"
;;

(* Funzione di verifica di tipo *)
let typecheck (x, y) = match x with 
  | "int" -> 
      (match y with 
       | Int(u) -> true
       | _ -> false)

  | "bool" -> 
      (match y with 
       | Bool(u) -> true
       | _ -> false)
      
  | "string" ->
      (match y with
       | String(u) -> true
       | _ -> false)
      
  | "float" ->
      (match y with
       | Float(u) -> true
       | _ -> false)

  | _ -> failwith ("not a valid type")
;;

(* Definizione di operazioni primari *)

let int_eq(x,y) =   
  match (typecheck("int",x), typecheck("int",y), x, y) with
  | (true, true, Int(v), Int(w)) -> Bool(v = w)
  | (_,_,_,_) -> failwith("run-time error ")
;;

let int_maj(x,y) =
  match (typecheck("int",x), typecheck("int",y), x, y) with
  | (true, true, Int(v), Int(w)) -> Bool(v > w)
  | (_,_,_,_) -> failwith("run-time error ")
;;

let int_inf(x,y) =
  match (typecheck("int",x), typecheck("int",y), x, y) with
  | (true, true, Int(v), Int(w)) -> Bool(v < w)
  | (_,_,_,_) -> failwith("run-time error ")
;;
       
let int_sum(x, y) = 
  match(typecheck("int",x), typecheck("int",y), x, y) with
  | (true, true, Int(v), Int(w)) -> Int(v + w)
  | (_,_,_,_) -> failwith("run-time error ")
;;

let int_sub(x, y) = 
  match(typecheck("int",x), typecheck("int",y), x, y) with
  | (true, true, Int(v), Int(w)) -> Int(v - w)
  | (_,_,_,_) -> failwith("run-time error ")
;;

let int_times(x, y) = 
  match(typecheck("int",x), typecheck("int",y), x, y) with
  | (true, true, Int(v), Int(w)) -> Int(v * w)
  | (_,_,_,_) -> failwith("run-time error ")
;;

let int_div(x, y) = 
  match (typecheck("int", x), typecheck("int", y), x, y) with
  | (true, true, Int(v), Int(w)) -> if w <> 0 then Int(v / w)
      else failwith("division by zero")
  | (_,_,_,_) -> failwith("run-time error")
;;

let int_mod(x,y) =
  match (typecheck("int", x), typecheck("int", y), x, y) with
  | (true, true, Int(v), Int(w)) -> if w <> 0 then Int(v mod w)
      else failwith("division by zero")
  | (_,_,_,_) -> failwith("run-time error")
;;

let float_eq(x,y) =   
  match (typecheck("float",x), typecheck("float",y), x, y) with
  | (true, true, Float(v), Float(w)) -> Bool(v = w)
  | (_,_,_,_) -> failwith("run-time error ")
;;

let float_maj(x,y) = 
  match (typecheck("float",x), typecheck("float",y), x, y) with
  | (true, true, Float(v), Float(w)) -> Bool(v > w)
  | (_,_,_,_) -> failwith("run-time error ")
;;

let float_inf(x,y) = 
  match (typecheck("float",x), typecheck("float",y), x, y) with
  | (true, true, Float(v), Float(w)) -> Bool(v < w)
  | (_,_,_,_) -> failwith("run-time error ")
;;
       
let float_sum(x, y) = 
  match(typecheck("float",x), typecheck("float",y), x, y) with
  | (true, true, Float(v), Float(w)) -> Float(v +. w)
  | (_,_,_,_) -> failwith("run-time error ")
;;

let float_sub(x, y) = 
  match(typecheck("float",x), typecheck("float",y), x, y) with
  | (true, true, Float(v), Float(w)) -> Float(v -. w)
  | (_,_,_,_) -> failwith("run-time error ")
;;

let float_times(x, y) = 
  match(typecheck("float",x), typecheck("float",y), x, y) with
  | (true, true, Float(v), Float(w)) -> Float(v *. w)
  | (_,_,_,_) -> failwith("run-time error ")
;;
  
let float_div(x, y) = 
  match (typecheck("float", x), typecheck("float", y), x, y) with
  | (true, true, Float(v), Float(w)) -> if w <> 0. then Float(v /. w)
      else failwith("divide by zero")
  | (_,_,_,_) -> failwith("run-time error")
;;

let string_eq(x, y) =
  match (typecheck("string", x), typecheck("string", y), x, y) with
  | (true, true, String(u), String(w)) -> Bool(u = w)
  | (_,_,_,_) -> failwith("run-time error ")
;;

let string_maj(x, y) =
  match (typecheck("string", x), typecheck("string", y), x, y) with
  | (true, true, String(u), String(w)) -> Bool(u > w)
  | (_,_,_,_) -> failwith("run-time error ")
;;

let string_inf(x,y) =
  match (typecheck("string", x), typecheck("string", y), x, y) with
  | (true, true, String(u), String(w)) -> Bool(u < w)
  | (_,_,_,_) -> failwith("run-time error ")
;;

let string_concat(x, y) = 
  match (typecheck("string", x), typecheck("string", y), x, y) with
  | (true, true, String(u), String(w)) -> String(u ^ w)
  | (_,_,_,_) -> failwith("run-time error ")
;;

let bool_and(x, y) = 
  match (typecheck("bool", x), typecheck("bool", y), x, y) with
  | (true, true, Bool(u), Bool(w)) -> Bool(u && w)
  | (_,_,_,_) -> failwith("run-time error ")
;;

let bool_or(x, y) = 
  match (typecheck("bool", x), typecheck("bool", y), x, y) with
  | (true, true, Bool(u), Bool(w)) -> Bool(u || w)
  | (_,_,_,_) -> failwith("run-time error ")
;;

let bool_not(x) = 
  match (typecheck("bool", x), x) with
  | (true, Bool(u)) -> Bool(not u)
  | (_,_) -> failwith("run-time error ")
;;

let max l = (match l with
    | [] -> failwith("bad parameter : empty list")
    | a::ll -> (let rec f lis a = match lis with
        | [] -> a
        | x::li -> if a > x then f li a
            else f li x
       in f ll a))
;;

let min l = (match l with
    | [] -> failwith("bad parameter : empty list")
    | a::ll -> (let rec f lis a = match lis with
        | [] -> a
        | x::li -> if a < x then f li a
            else f li x
       in f ll a))
;;

let rec search a l = (match l with
    | [] -> false
    | x::ll -> if x = a then true
        else search a ll)
;;

let rec cancDup myList = (match myList with
    | [] -> []
    | a::l -> if search a l then cancDup l
        else a::(cancDup l))
;;

let rec union l1 l2 = (match (l1, l2) with
    | (l, []) -> l
    | ([], l) -> l
    | (a::ll, l) -> if search a l then union ll l
        else a::(union ll l))
;;

let rec inter l1 l2 = (match (l1, l2) with
    | ([], l) -> []
    | (l, []) -> []
    | (a::ll, l) -> if search a l then (a::(inter ll l))
        else inter ll l)
;;

let rec remove a l = (match l with
    | [] -> []
    | x::ll -> if x = a then ll
        else x::(remove a ll))
;;


let rec diff l1 l2 = ( match (l1, l2) with
    | ([], l) -> []
    | (l, []) -> l
    | (a::ll, l) -> if search a l then (diff ll l)
        else a::(diff ll l) )
;;

let rec sub l1 l2 = (match (l1, l2) with 
    | (_, []) -> false
    | ([], l) -> true
    | (a::ll, l) -> (search a l) && (sub ll l))
;;

let andList l = (match l with
    | [] -> failwith("empty list")
    | a::ll -> let rec f lis x = match lis with
        | [] -> x
        | y::tail -> match (x, y) with
          | (Bool(b1), Bool(b2)) -> f tail (Bool(b1 && b2))
          | _ -> failwith("type error")
        in f ll a)
;;

let hd l = (match l with
	| [] -> failwith("empty list")
	| x::ll -> x)
;;

let rec tl l = (match l with
	| [] -> failwith("empty list")
	| x::ll -> if ll = [] then x
				else tl ll)
;;
  
(* Interprete full *)

let rec eval  (e:exp) (s:evT env) = match e with
  | EInt(n) -> Int(n) 
  | EFloat(n) -> Float(n)
  | EString(u) -> String(u)
  | ETrue -> Bool(true)
  | EFalse -> Bool(false)
  | Eq(e1, e2) -> (let ev1 = eval e1 s in
                   let ev2 = eval e2 s in
                   match (ev1, ev2) with
                   | (Int(u), Int(v)) -> int_eq(Int(u), Int(v))
                   | (Float(u), Float(v)) -> float_eq(Float(u), Float(v))
                   | (String(u), String(v)) -> string_eq(String(u), String(v))
                   | (_,_) -> failwith("type error"))
  | Maj(e1, e2) -> (let ev1 = eval e1 s in
                    let ev2 = eval e2 s in
                    match (ev1, ev2) with
                    | (Int(u), Int(v)) -> int_maj(Int(u), Int(v))
                    | (Float(u), Float(v)) -> float_maj(Float(u), Float(v))
                    | (String(u), String(v)) -> string_maj(String(u), String(v))
                    | (_,_) -> failwith("type error"))
  | Inf(e1, e2) -> (let ev1 = eval e1 s in
                    let ev2 = eval e2 s in
                    match (ev1, ev2) with
                    | (Int(u), Int(v)) -> int_inf(Int(u), Int(v))
                    | (Float(u), Float(v)) -> float_inf(Float(u), Float(v))
                    | (String(u), String(v)) -> string_inf(String(u), String(v))
                    | (_,_) -> failwith("type error"))
  | Times(e1,e2) -> (let ev1 = eval e1 s in
                     let ev2 = eval e2 s in
                     match (ev1, ev2) with
                     | (Int(u), Int(v)) -> int_times(Int(u), Int(v))
                     | (Float(u), Float(v)) -> float_times(Float(u), Float(v))
                     | (_,_) -> failwith("type error"))
  | Div(e1, e2) -> (let ev1 = eval e1 s in
                    let ev2 = eval e2 s in
                    match (ev1, ev2) with
                    | (Int(u), Int(v)) -> int_div(Int(u), Int(v))
                    | (Float(u), Float(v)) -> float_div(Float(u), Float(v))
                    | (_,_) -> failwith("type error"))
  | Sum(e1, e2) -> (let ev1 = eval e1 s in
                    let ev2 = eval e2 s in
                    match (ev1, ev2) with
                    | (Int(u), Int(v)) -> int_sum(Int(u), Int(v))
                    | (Float(u), Float(v)) -> float_sum(Float(u), Float(v))
                    | (_,_) -> failwith("type error"))
  | Sub(e1, e2) -> (let ev1 = eval e1 s in
                    let ev2 = eval e2 s in
                    match (ev1, ev2) with
                    | (Int(u), Int(v)) -> int_sub(Int(u), Int(v))
                    | (Float(u), Float(v)) -> float_sub(Float(u), Float(v))
                    | (_,_) -> failwith("type error"))
  | Mod(e1, e2) -> (let ev1 = eval e1 s in
                    let ev2 = eval e2 s in
                    match (ev1, ev2) with
                    | (Int(u), Int(v)) -> int_mod(Int(u), Int(v))
                    | (_,_) -> failwith("type error"))
  | Concat(e1, e2) -> (let ev1 = eval e1 s in
                       let ev2 = eval e2 s in
                       match (ev1, ev2) with
                       | (String(u), String(v)) -> string_concat(String(u), String(v))
                       | (_,_) -> failwith("type error"))
  | And(e1, e2) -> (let ev1 = eval e1 s in
                    let ev2 = eval e2 s in
                    match (ev1, ev2) with
                    | (Bool(u), Bool(v)) -> Bool(u && v)
                    | (_,_) -> failwith("type error"))
  | Or(e1, e2) -> (let ev1 = eval e1 s in
                   let ev2 = eval e2 s in
                   match (ev1, ev2) with
                   | (Bool(u), Bool(v)) -> Bool(u || v)
                   | (_,_) -> failwith("type error"))
  | Not(e1) -> (let ev1 = eval e1 s in 
                match (ev1) with
                | (Bool(u)) -> Bool(not u)
                | (_) -> failwith("type error"))
  | Ifthenelse(e1,e2,e3) -> (let g = eval e1 s in
                             let ev2 = eval e2 s in
                             let ev3 = eval e3 s in
                             if (typeof ev2) = (typeof ev3) then
                               (match (typecheck("bool", g), g) with
                                | (true, Bool(true)) -> ev2
                                | (true, Bool(false)) -> ev3
                                | (_, _) -> failwith ("nonboolean guard"))
                             else failwith("type error"))
  | Den(i) -> lookup s i
  | Let(i, e, ebody) -> eval ebody (bind s i (eval e s))
  | Fun(arg, ebody) -> Closure(arg,ebody,s)
  | Letrec(f, arg, fBody, letBody) -> 
      let benv = bind (s) (f) (RecClosure(f, arg, fBody,s)) in
      eval letBody benv
  | Apply(eF, eArg) ->
      let fclosure = eval eF s in 
      (match fclosure with 
       | Closure(arg, fbody, fDecEnv) ->
           let aVal = eval eArg s in
           let aenv = bind fDecEnv arg aVal in 
           eval fbody aenv
       | RecClosure(f, arg, fbody, fDecEnv) ->
           let aVal = eval eArg s in
           let rEnv = bind fDecEnv f fclosure in
           let aenv = bind rEnv arg aVal in 
           eval fbody aenv
       | _ -> failwith("non functional value"))

  | EmptySet(typ) -> (match typ with
      | t when ((t = "int") || (t = "float") || (t = "string"))
        -> SetT(t, [])
      | _ -> failwith("Error type"))
                       
  | Singleton(v, tip) -> (let ev = eval v s in
                          match (ev, tip) with
                          | (Int n, "int") -> SetT("int", (Int n)::[])
                          | (Float n, "float") -> SetT("float", (Float n)::[])
                          | (String s, "string") -> SetT("string", (String s)::[])
                          | (_,_) -> failwith("Error type"))
                                            
  | Of(typ, lv) -> ( match typ with
      | "int" | "float" | "string" -> let l = ofSet typ lv s in
          SetT(typ, cancDup l)
      | _ -> failwith("type error")) 
  | Union(s1, s2) -> ( let es1 = eval s1 s in
                       let es2 = eval s2 s in
                       match (es1, es2) with
                       | (SetT("int", l1), SetT("int", l2)) -> SetT("int", union l1 l2)
                       | (SetT("float", l1), SetT("float", l2)) -> SetT("float", union l1 l2)
                       | (SetT("string", l1), SetT("string", l2)) -> SetT("string", union l1 l2)
                       | (_,_) -> failwith("type error")) 
                     
  | Inter(s1, s2) -> ( let es1 = eval s1 s in
                       let es2 = eval s2 s in
                       match (es1, es2) with
                       | (SetT("int", l1), SetT("int", l2)) -> SetT("int", inter l1 l2)
                       | (SetT("float", l1), SetT("float", l2)) -> SetT("float", inter l1 l2)
                       | (SetT("string", l1), SetT("string", l2)) -> SetT("string", inter l1 l2)
                       | (_,_) -> failwith("type error"))
                     
  | Diff(s1, s2) -> ( let es1 = eval s1 s in
                      let es2 = eval s2 s in
                      match (es1, es2) with
                      | (SetT("int", l1), SetT("int", l2)) -> SetT("int", diff l1 l2)
                      | (SetT("float", l1), SetT("float", l2)) -> SetT("float", diff l1 l2)                                    
                      | (SetT("string", l1), SetT("string", l2)) -> SetT("string", diff l1 l2)
                      | (_,_) -> failwith("type error"))
                    
  | Insert(set, a) -> (match (eval a s, eval set s) with
      | (Int u, SetT("int", l)) -> if search (Int u) l then SetT("int", l)
          else SetT("int", (Int u)::l)
      | (Float u, SetT("float", l)) -> if search (Float u) l then SetT("float", l)
          else SetT("float", (Float u)::l)
      | (String u, SetT("string", l)) -> if search (String u) l then SetT("string", l)
          else SetT("string", (String u)::l)
      | (_,_) -> failwith("type error"))
                    
  | RemoveSet(set, a) -> (match (eval a s, eval set s) with
      | (Int u, SetT("int", l)) -> SetT("int", (remove (Int u) l))
      | (Float u, SetT("float", l)) -> SetT("float", (remove (Float u) l))
      | (String u, SetT("string", l)) -> SetT("string", (remove (String u) l))
      | (_,_) -> failwith("type error"))
    
  | IsEmpty(set) -> (match (eval set s) with
      | SetT(_, l) -> if l = [] then Bool(true)
          else Bool(false)
      | _ -> failwith("type error"))
    
  | HasElement(set, a) -> (match (eval a s, eval set s) with
      | (Int u, SetT("int", l)) -> Bool(search (Int u) l)
      | (Float u, SetT("float", l)) -> Bool(search (Float u) l)
      | (String u, SetT("string", l)) -> Bool(search (String u) l)
      | _ -> failwith("type error"))
    
  | IsSubSet(set1, set2) -> (match (eval set1 s, eval set2 s) with
      | (SetT("int", l1), SetT("int", l2)) -> Bool(sub l1 l2)
      | (SetT("float", l1), SetT("float", l2)) -> Bool(sub l1 l2)
      | (SetT("string", l1), SetT("string", l2)) -> Bool(sub l1 l2)
      | (_,_) -> failwith("type error"))
    
  | MaxSet(set) -> (match (eval set s) with
      | (SetT(typ, l)) when ((typ = "int") || (typ = "float") || (typ = "string"))
        -> if l = [] then failwith("error : empty set")
        	else max l 
      | _ -> failwith("type error"))
  | MinSet(set) -> (match (eval set s) with
      | (SetT(typ, l)) when ((typ = "int") || (typ = "float") || (typ = "string"))
        -> if l = [] then failwith("error : empty set")
        	else min l 
      | _ -> failwith("type error"))
                    
  | ForAll(predicate, aset) -> (let fclosure = eval predicate s in
                                let eset = eval aset s in
                                match eset with
                                | (SetT(typ, l)) when ((typ = "int") || (typ = "float") || (typ = "string")) ->
                                    (match fclosure with
                                     | Closure(arg, ebody, s) ->
                                         let myList = applyToSet fclosure l in
                                         let rec f lis x = match lis with
                                           | [] -> (if (x = Bool(true)) || (x = Bool(false)) then x
                                                    else failwith("Type error : the given function is not a predicate"))
                                           | y::tail -> match (x, y) with
                                             | (Bool(b1), Bool(b2)) ->
                                             	if ((b1 = false) || (b2 = false)) then Bool(false)
                                             	else f tail (Bool(true))
                                             | _ -> failwith("Type error : the given function is not a predicate")
                                         in (match myList with
                                             | [] -> Bool(true)
                                             | a::tail -> f tail a)
                                     | _ -> failwith("not predicate")) 
                                | _ -> failwith("type error"))
                          
  | Exists(predicate, aset) -> (let fclosure = eval predicate s in
                                let eset = eval aset s in
                                match eset with
                                | (SetT(typ, l)) when ((typ = "int") || (typ = "float") || (typ = "string"))
                                  -> (match fclosure with
                                      | Closure(arg, ebody, s) ->
                                          let myList = applyToSet fclosure l in
                                          let rec f lis x = match lis with
                                            | [] -> (if (x = Bool(true)) || (x = Bool(false)) then x
                                                     else failwith("Type error : the given function is not a predicate"))
                                            | y::tail -> match (x, y) with
                                              | (Bool(b1), Bool(b2)) ->
                                              		if ((b1 = true) || b2 = true) then Bool(true)
                                              		else f tail (Bool(false))
                                              | _ -> failwith("Type error : the given function is not a predicate")
                                          in (match myList with
                                              | [] -> Bool(false)
                                              | a::tail -> f tail a)
                                      | _ -> failwith("not predicate"))
                                  
                                | _ -> failwith("type error"))           

  | Filter(predicate, aset) -> (let fclosure = eval predicate s in
                                let eset = eval aset s in
                                match eset with
                                | (SetT(typ, l)) when ((typ = "int") || (typ = "float") || (typ = "string")) ->
                                    (match fclosure with
                                     | Closure(arg, ebody, s) ->
                                         SetT(typ, applyFunForFilter fclosure l)
                                     | _ -> failwith("not predicate"))
                                | _ -> failwith("type error"))                         

  | Map(functio, aset) -> (let fclosure = eval functio s in
                           let eset = eval aset s in
                           match eset with
                           | (SetT(typ, l)) when ((typ = "int") || (typ = "float") || (typ = "string"))
                             -> (match fclosure with
                                 | Closure(arg, ebody, s) ->
                                 	let listResult = applyFunForMap fclosure l in
                                     	if listResult = [] then SetT(typ, [])
                                 	 	else let tp = typeof (hd listResult) in
                                 	 		if ((tp = "int") || (tp = "float") || (tp = "string"))
                                 	 			then SetT(typeof (hd listResult), listResult)
                                 	 		else failwith("bad type result")
                                 | RecClosure(g, arg, ebody, s) ->
                                     let listResult = applyFunForMap fclosure l in
                                     	if listResult = [] then SetT(typ, [])
                                 	 	else let tp = typeof (hd listResult) in
                                 	 		if ((tp = "int") || (tp = "float") || (tp = "string"))
                                 	 			then SetT(typeof (hd listResult), listResult)
                                 	 		else failwith("bad type result")
                                 | _ -> failwith("not function"))
                           | _ -> failwith("type error"))                         
  
(* Definzione di funzioni auxiliari usati nel eval e legati ad essi *)

and ofSet tip l s = (match l with
    | [] -> []
    | a::ll -> (let ea = eval a s in
                if typecheck(tip, ea) then ea::(ofSet tip ll s)
                else failwith("type error")))

and applyFunForAll (f:evT) (l : evT list) : bool = (match l with
    | [] -> true
    | a::ll -> false)

and valFun f ss = (let closure = eval f ss in
                   match closure with
                   | Closure(arg, fBody, fDecEnv)
                     -> Closure (arg, fBody, fDecEnv)
                   | RecClosure(g, arg, fBody, fDecEnv)
                     -> Closure(arg, fBody, (bind fDecEnv g closure))
                   | _ -> failwith("non functional value"))

and applyToSet f l = (match l with
    | [] -> []
    | a::ll -> let res = (match f with
        | Closure(arg, fBody, fDecEnv) -> eval fBody (bind fDecEnv arg a)
        | RecClosure(g, arg, fBody, fDecEnv) -> eval fBody (bind fDecEnv arg a)
        | _ -> failwith("type error")) 
        in (res)::(applyToSet f ll))

and applyFunForFilter f l = (match l with 
    | [] -> []
    | a::ll -> match f with
      | Closure(arg, fBody, fDecEnv) -> let b = eval fBody (bind fDecEnv arg a) in
          if b = Bool(true) then a::(applyFunForFilter f ll)
          else (applyFunForFilter f ll)
      | RecClosure(g, arg, fBody, fDecEnv) -> let b = eval fBody (bind fDecEnv arg a) in
          if b = Bool(true) then a::(applyFunForFilter f ll)
          else (applyFunForFilter f ll)
      | _ -> failwith("type error"))
  
and applyFunForMap f l = (match l with
    | [] -> []
    | a::ll -> let res = (match f with
        | Closure(arg, fBody, fDecEnv) -> eval fBody (bind fDecEnv arg a)
        | RecClosure(g, arg, fBody, fDecEnv) -> eval fBody (bind fDecEnv arg a)
        | _ -> failwith("type error"))
        in (res)::(applyFunForMap f ll))
;;