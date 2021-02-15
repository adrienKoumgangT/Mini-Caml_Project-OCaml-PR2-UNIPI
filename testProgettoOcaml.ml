(*  Test dei costruttori di set:  *)

(* "test dei costruttori di set" *)

(* "test del costruttore EmptySet" *)

let setEmpty1 = EmptySet("int");;
eval setEmpty1 emptyEnv;;

let setEmpty2 = EmptySet("float");;
eval setEmpty2 emptyEnv;;

let setEmpty3 = EmptySet("string");;
eval setEmpty3 emptyEnv;;

let setEmpty4 = EmptySet("bool");;
eval setEmpty4 emptyEnv;;

let setEmpty5 = EmptySet("");;
eval setEmpty5 emptyEnv;;

(* "test del costruttore Singleton" *)

let setSingleton1 = Singleton(EInt(9), "int");;
eval setSingleton1 emptyEnv;;

let setSingleton2 = Singleton(EFloat(9.9), "float");;
eval setSingleton2 emptyEnv;;

let setSingleton3 = Singleton(EString("adrien"), "string");;
eval setSingleton3 emptyEnv;;

let setSingleton4 = Singleton(Times(EInt(3), EInt(3)), "int");;
eval setSingleton4 emptyEnv;;

let setSingleton5 = Singleton(Div(EFloat(90.0), EFloat(10.0)), "float");;
eval setSingleton5 emptyEnv;;

let setSingleton6 = Singleton(Concat(EString("Salve "), EString("prof")), "string");;
eval setSingleton6 emptyEnv;;

let setSingleton7 = Singleton(ETrue, "bool");;
eval setSingleton7 emptyEnv;;

let setSingleton8 = Singleton(EFloat(9.1), "int");;
eval setSingleton8 emptyEnv;; 

let setSingleton9 = Singleton(EString("adrien"), "float");;
eval setSingleton9 emptyEnv;;


(* "test del costruttore of" *)

let setOf1 = Of("int", [EInt(9); EInt(99)]);;
eval setOf1 emptyEnv;;

let setOf2 = Of("float", [EFloat(9.0); EFloat(99.0)]);;
eval setOf2 emptyEnv;;

let setOf3 = Of("string", [EString("salve"); EString("prof")]);;
eval setOf3 emptyEnv;;

let setOf4 = Of("int", [Sum(EInt(1), EInt(2)); Times(EInt(3), EInt(4)); EInt(99)]);;
eval setOf4 emptyEnv;;

let setOf5 = Of("", []);;
eval setOf5 emptyEnv;;

let setOf6 = Of("string", [Concat(EString("ciao "), EString("prof")); EString("adrien")]);;
eval setOf6 emptyEnv;;

let setOf7 = Of("int", [EInt(9); EInt(11); EString("adrien")]);;
eval setOf7 emptyEnv;;

let setOf8 = Of("float", [EFloat(9.0); EInt(1); EFloat(3.0)]);;
eval setOf8 emptyEnv;;

let setOf9 = Of("float", [EFloat(9.0); Mod(EInt(10), EInt(3))]);;
eval setOf9 emptyEnv;;



(* "test degli operazioni di base" *)

let setInt1 = Of("int", [EInt(1); EInt(3); EInt(5)]);;
let setInt2 = Of("int", [EInt(7); EInt(-2); EInt(4); EInt(1)]);;
let setInt3 = EmptySet("int");;

let setFloat1 = Of("float", [EFloat(4.3); EFloat(3.5); EFloat(5.9)]);;
let setFloat2 = Of("float", [EFloat(7.); EFloat(2.2); EFloat(4.3); EFloat(-1.2); EFloat(3.5)]);;
let setFloat3 = EmptySet("float");;

let setString1 = Of("string", [EString("adrien"); EString("adrian"); EString("adri")]);;
let setString2 = Of("string", [EString("prof"); EString("adri"); EString("ciao"); EString("salve")]);;
let setString3 = EmptySet("string");;


(* "test dell'operazione di base UNION" *)

let union1 = Union(setInt1, setInt2);;
eval union1 emptyEnv;;

let union2 = Union(setInt1, setInt3);;
eval union2 emptyEnv;;

let union3 = Union(setFloat1, setFloat2);;
eval union3 emptyEnv;;

let union4 = Union(setFloat1, setFloat3);;
eval union4 emptyEnv;;

let union5 = Union(setString1, setString2);;
eval union5 emptyEnv;;

let union6 = Union(setString1, setString3);;
eval union6 emptyEnv;;

let union7 = Union(setInt1, setFloat1);;
eval union7 emptyEnv;;

let union8 = Union(setFloat2, setString1);;
eval union8 emptyEnv;;

let union9 = Union(setInt3, setFloat3);;
eval union9 emptyEnv;;

(* "test dell'operazione di base INTER" *)

let inter1 = Inter(setInt1, setInt2);;
eval inter1 emptyEnv;;

let inter2 = Inter(setInt1, setInt3);;
eval inter2 emptyEnv;;

let inter3 = Inter(setFloat1, setFloat2);;
eval inter3 emptyEnv;;

let inter4 = Inter(setFloat1, setFloat3);;
eval inter4 emptyEnv;;

let inter5 = Inter(setString1, setString2);;
eval inter5 emptyEnv;;

let inter6 = Inter(setString1, setString3);;
eval inter6 emptyEnv;;

let inter7 = Inter(setInt1, setFloat1);;
eval inter7 emptyEnv;;

let inter8 = Inter(setFloat1, setString1);;
eval inter8 emptyEnv;;

let inter9 = Inter(setInt3, setFloat3);;
eval inter9 emptyEnv;;

(* "test dell'operazione di base DIFF" *)

let diff1 = Diff(setInt1, setInt2);;
eval diff1 emptyEnv;;

let diff2 = Diff(setInt1, setInt3);;
eval diff2 emptyEnv;;

let diff3 = Diff(setFloat1, setFloat2);;
eval diff3 emptyEnv;;

let diff4 = Diff(setFloat3,setFloat1 );;
eval diff4 emptyEnv;;

let diff5 = Diff(setString1, setString2);;
eval diff5 emptyEnv;;

let diff6 = Diff(setString1, setString3);;
eval diff6 emptyEnv;;

let diff7 = Diff(setInt1, setFloat1);;
eval diff7 emptyEnv;;

let diff8 = Diff(setInt1, setString2);;
eval diff8 emptyEnv;;

let diff9 = Diff(setInt3, setFloat3);;
eval diff9 emptyEnv;;

(* "test dell'operazione INSERT" *)

let insert1 = Insert(setInt1, EInt(9));;
eval insert1 emptyEnv;;

let insert2 = Insert(setInt1, EInt(1));;
eval insert2 emptyEnv;;

let insert3 = Insert(setFloat1, EFloat(9.9));;
eval insert3 emptyEnv;;

let insert4 = Insert(setFloat1, EFloat(5.9));;
eval insert4 emptyEnv;;

let insert5 = Insert(setString1, EString("adriano"));;
eval insert5 emptyEnv;;

let insert6 = Insert(setString1, EString("adrian"));;
eval insert6 emptyEnv;;

let insert7 = Insert(setInt1, EFloat(9.1));;
eval insert7 emptyEnv;;

let insert8 = Insert(setFloat1, EInt(3));;
eval insert8 emptyEnv;;

let insert9 = Insert(setString1, EFloat(9.1));;
eval insert9 emptyEnv;;

(* "test dell'operazione REMOVE" *)

let remove1 = RemoveSet(setInt1, EInt(9));;
eval remove1 emptyEnv;;

let remove2 = RemoveSet(setInt1, EInt(1));;
eval remove2 emptyEnv;;

let remove3 = RemoveSet(setFloat1, EFloat(9.9));;
eval remove3 emptyEnv;;

let remove4 = RemoveSet(setFloat1, EFloat(5.9));;
eval remove4 emptyEnv;;

let remove5 = RemoveSet(setString1, EString("adriano"));;
eval remove5 emptyEnv;;

let remove6 = RemoveSet(setString1, EString("adrian"));;
eval remove6 emptyEnv;;

let remove7 = RemoveSet(setInt1, EFloat(9.1));;
eval remove7 emptyEnv;;

let remove8 = RemoveSet(setFloat1, EInt(3));;
eval remove8 emptyEnv;;

let remove9 = RemoveSet(setString1, EFloat(9.1));;
eval remove9 emptyEnv;;

(* "test dell'operazione ISEMPTY" *)

let isEmpty1 = IsEmpty(setInt1);;
eval isEmpty1 emptyEnv;;

let isEmpty2 = IsEmpty(setInt3);;
eval isEmpty2 emptyEnv;;

let isEmpty3 = IsEmpty(setFloat1);;
eval isEmpty3 emptyEnv;;

let isEmpty4 = IsEmpty(setFloat3);;
eval isEmpty4 emptyEnv;;

let isEmpty5 = IsEmpty(setString1);;
eval isEmpty5 emptyEnv;;

let isEmpty6 = IsEmpty(setString3);;
eval isEmpty6 emptyEnv;;

(* "test dell'operazione HASELEMENT" *)

let hasElement1 = HasElement(setInt1, EInt(9));;
eval hasElement1 emptyEnv;;

let hasElement2 = HasElement(setInt1, EInt(1));;
eval hasElement2 emptyEnv;;

let hasElement3 = HasElement(setFloat1, EFloat(9.9));;
eval hasElement3 emptyEnv;;

let hasElement4 = HasElement(setFloat1, EFloat(5.9));;
eval hasElement4 emptyEnv;;

let hasElement5 = HasElement(setString1, EString("adriano"));;
eval hasElement5 emptyEnv;;

let hasElement6 = HasElement(setString1, EString("adrian"));;
eval hasElement6 emptyEnv;;

let hasElement7 = HasElement(setInt1, EFloat(9.1));;
eval hasElement7 emptyEnv;;

let hasElement8 = HasElement(setFloat1, EInt(3));;
eval hasElement8 emptyEnv;;

let hasElement9 = HasElement(setString1, EFloat(9.1));;
eval hasElement9 emptyEnv;;

(* "test dell'operazione ISSUB" *)

let isSub1 = IsSubSet(setInt1, Singleton(EInt(1), "int"));;
eval isSub1 emptyEnv;;

let isSub2 = IsSubSet(setInt1, Singleton(EInt(35), "int"));;
eval isSub2 emptyEnv;;

let isSub3 = IsSubSet(setInt1, setInt3);;
eval isSub3 emptyEnv;;

let isSub4 = IsSubSet(setFloat1, Singleton(EFloat(4.3), "float"));;
eval isSub4 emptyEnv;;

let isSub5 = IsSubSet(setFloat1, Singleton(EFloat(23.7), "float"));;
eval isSub5 emptyEnv;;

let isSub6 = IsSubSet(setFloat1, setFloat3);;
eval isSub6 emptyEnv;;

let isSub7 = IsSubSet(setString1, Singleton(EString("adrian"), "string"));;
eval isSub7 emptyEnv;;

let isSub8 = IsSubSet(setString1, Singleton(EString("mondo"), "string"));;
eval isSub8 emptyEnv;;

let isSub9 = IsSubSet(setString1, setString3);;
eval isSub9 emptyEnv;;

(* "test dell'operazione MAX" *)

let max1 = MaxSet(setInt1);;
eval max1 emptyEnv;;

let max2 = MaxSet(setInt3);;
eval max2 emptyEnv;;

let max3 = MaxSet(setFloat1);;
eval max3 emptyEnv;;

let max4 = MaxSet(setFloat3);;
eval max4 emptyEnv;;

let max5 = MaxSet(setString1);;
eval max5 emptyEnv;;

let max6 = MaxSet(setString3);;
eval max6 emptyEnv;;

(* "test dell'operazione MIN" *)

let min1 = MinSet(setInt1);;
eval min1 emptyEnv;;

let min2 = MinSet(setInt3);;
eval min2 emptyEnv;;

let min3 = MinSet(setFloat1);;
eval min3 emptyEnv;;

let min4 = MinSet(setFloat3);;
eval min4 emptyEnv;;

let min5 = MinSet(setString1);;
eval min5 emptyEnv;;

let min6 = MinSet(setString3);;
eval min6 emptyEnv;;


(* "definizione di 3 predicati" *)

let pred1 = Fun("x", Maj(Den("x"), EInt(0)));;
eval pred1 (bind emptyEnv "x" EInt(9));;
let pred2 = Fun("x", Maj(Den("x"), EFloat(0.)));;
eval pred2 (bind emptyEnv "x" EFloat(9.));;
let pred3 = Fun("x", Maj(Den("x"), EString("toto")));;
eval pred3 (bind emptyEnv "x" EString("adrian"));;


(* "test dell'operazione FORALL" *)

let forAll1 = ForAll(pred1, setInt1);;
eval forAll1 emptyEnv;;

let forAll2 = ForAll(pred1, setInt3);;
eval forAll2 emptyEnv;;

let forAll3 = ForAll(pred2, setFloat1);;
eval forAll3 emptyEnv;;

let forAll4 = ForAll(pred2, setFloat3);;
eval forAll4 emptyEnv;;

let forAll5 = ForAll(pred3, setString1);;
eval forAll5 emptyEnv;;

let forAll6 = ForAll(pred3, setString3);;
eval forAll6 emptyEnv;;

let forAll7 = ForAll(pred1, setFloat3);;
eval forAll7 emptyEnv;;

let forAll8 = ForAll(pred2, setInt1);;
eval forAll8 emptyEnv;;

let forAll9 = ForAll(pred1, setString1);;
eval forAll9 emptyEnv;;

(* "test dell'operazione EXISTS" *)

let exist1 = Exists(pred1, setInt1);;
eval exist1 emptyEnv;;

let exist2 = Exists(pred1, setInt3);;
eval exist2 emptyEnv;;

let exist3 = Exists(pred2, setFloat1);;
eval exist3 emptyEnv;;

let exist4 = Exists(pred2, setFloat3);;
eval exist4 emptyEnv;;

let exist5 = Exists(pred3, setString1);;
eval exist5 emptyEnv;;

let exist6 = Exists(pred3, setString3);;
eval exist6 emptyEnv;;

let exist7 = Exists(pred1, setFloat3);;
eval exist7 emptyEnv;;

let exist8 = Exists(pred2, setInt1);;
eval exist8 emptyEnv;;

let exist9 = Exists(pred1, setString1);;
eval exist9 emptyEnv;;

(* "test dell'operazione FILTER" *)

let filter1 = Filter(pred1, setInt1);;
eval filter1 emptyEnv;;

let filter2 = Filter(pred1, setInt3);;
eval filter2 emptyEnv;;

let filter3 = Filter(pred2, setFloat1);;
eval filter3 emptyEnv;;

let filter4 = Filter(pred2, setFloat3);;
eval filter4 emptyEnv;;

let filter5 = Filter(pred3, setString1);;
eval filter5 emptyEnv;;

let filter6 = Filter(pred3, setString3);;
eval filter6 emptyEnv;;

let filter7 = Filter(pred1, setFloat3);;
eval filter7 emptyEnv;;

let filter8 = Filter(pred2, setInt1);;
eval filter8 emptyEnv;;

let filter9 = Filter(pred1, setString1);;
eval filter9 emptyEnv;;


(* "definizione di 3 funzioni" *)

let fun1 = Fun("x", Sum(Den("x"), EInt(100)));;
let fun2 = Fun("", Times(Den("x"), EFloat(100.)));;
let fun3 = Fun("x", Concat(Den("x"), EString(" : so good!")));;

(* "test dell'operazione MAP" *)


let map1 = Map(fun1, setInt1);;
eval map1 emptyEnv;;

let map2 = Map(fun1, setInt3);;
eval map2 emptyEnv;;

let map3 = Map(fun2, setFloat1);;
eval map3 emptyEnv;;

let map4 = Map(fun2, setFloat3);;
eval map4 emptyEnv;;

let map5 = Map(fun3, setString1);;
eval map5 emptyEnv;;

let map6 = Map(fun3, setString3);;
eval map6 emptyEnv;;

let map7 = Map(fun1, setFloat1);;
eval map7 emptyEnv;;

let map8 = Map(fun1, setFloat3);;
eval map8 emptyEnv;;

let map9 = Map(fun3, setInt1);;
eval map9 emptyEnv;;

let map10 = Map(fun3, setInt3);;
eval map10 emptyEnv;;


(* "test finito" *)