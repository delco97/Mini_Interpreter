type ide = string;; 
type exp = Eint of int | Ebool of bool | Den of ide | Prod of exp * exp | Sum of exp * exp | Diff of exp * exp | Eq of exp * exp |
	Minus of exp | IsZero of exp | Or of exp * exp | And of exp * exp | Not of exp | Ifthenelse of exp * exp * exp |
	Let of ide * exp * exp | Fun of ide * exp | FunCall of exp * exp | Letrec of ide * exp * exp | 
	ETree of tree | 
	ApplyOver of (ide list) * exp * exp |
	Select of ide * exp 
	and tree = Empty | Node of ide * exp * tree * tree;;
	
(*ambiente polimorfo*)
type 't env = ide -> 't;;
let emptyenv (v : 't) = function x -> v;;
let applyenv (r : 't env) (i : ide) = r i;;
let bind (r : 't env) (i : ide) (v : 't) = function x -> if x = i then v else applyenv r x;;

(*tipi esprimibili
Gli alberi sono considerati come tipi esprimibili, in quanto la valutazione di Select restituisce un albero
*)
type evT = Int of int | Bool of bool | Tree of evTree | Unbound | FunVal of evFun | RecFunVal of ide * evFun
and evFun = ide * exp * evT env
and evTree = Empty | Node of ide * evT * evT * evT;;

(*rts*)
(*type checking*)
let typecheck (s : string) (v : evT) : bool = match s with
	"int" -> (match v with
		Int(_) -> true |
		_ -> false) |
	"bool" -> (match v with
		Bool(_) -> true |
		_ -> false) |
	"tree" -> (match v with
		Tree(_) -> true |
		_ -> false) |		
	_ -> failwith("not a valid type");;

(*funzioni primitive*)
let prod x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		(Int(n),Int(u)) -> Int(n*u) |
		_ -> failwith("Type error - typecheck failed"))
	else failwith("Type error");;

let sum x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		(Int(n),Int(u)) -> Int(n+u) |
		_ -> failwith("Type error - typecheck failed"))
	else failwith("Type error");;

let diff x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		(Int(n),Int(u)) -> Int(n-u) |
		_ -> failwith("Type error - typecheck failed"))
	else failwith("Type error");;

let eq x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		(Int(n),Int(u)) -> Bool(n=u) |
		_ -> failwith("Type error - typecheck failed"))
	else failwith("Type error");;

let minus x = if (typecheck "int" x) 
	then (match x with
	   	Int(n) -> Int(-n) |
		_ -> failwith("Type error - typecheck failed"))
	else failwith("Type error");;

let iszero x = if (typecheck "int" x)
	then (match x with
		Int(n) -> Bool(n=0) |
		_ -> failwith("Type error - typecheck failed"))
	else failwith("Type error");;

let vel x y = if (typecheck "bool" x) && (typecheck "bool" y)
	then (match (x,y) with
		(Bool(b),Bool(e)) -> (Bool(b||e)) |
		_ -> failwith("Type error - typecheck failed"))
	else failwith("Type error");;

let et x y = if (typecheck "bool" x) && (typecheck "bool" y)
	then (match (x,y) with
		(Bool(b),Bool(e)) -> Bool(b&&e) |
		_ -> failwith("Type error - typecheck failed"))
	else failwith("Type error");;

let non x = if (typecheck "bool" x)
	then (match x with
		Bool(true) -> Bool(false) |
		Bool(false) -> Bool(true) |
		_ -> failwith("Type error - typecheck failed"))
	else failwith("Type error");;

(*interprete*)
let rec eval (e : exp) (r : evT env) : evT = match e with
	Eint n -> Int n |
	Ebool b -> Bool b |
	ETree(Empty) -> Tree(Empty) |
	ETree(Node(i,en,t1,t2)) ->	let ris1 = eval (ETree(t1)) r in
							   	let ris2 = eval (ETree(t2)) r in
								let ris_en = eval en r in
								Tree(Node(i,ris_en,ris1, ris2)) |
	IsZero a -> iszero (eval a r) |
	Den i -> applyenv r i |
	Eq(a, b) -> eq (eval a r) (eval b r) |
	Prod(a, b) -> prod (eval a r) (eval b r) |
	Sum(a, b) -> sum (eval a r) (eval b r) |
	Diff(a, b) -> diff (eval a r) (eval b r) |
	Minus a -> minus (eval a r) |
	And(a, b) -> et (eval a r) (eval b r) |
	Or(a, b) -> vel (eval a r) (eval b r) |
	Not a -> non (eval a r) |
	Ifthenelse(a, b, c) -> 
		let g = (eval a r) in
			if (typecheck "bool" g) 
				then (if g = Bool(true) then (eval b r) else (eval c r))
				else failwith ("nonboolean guard") |
	Let(i, e1, e2) -> eval e2 (bind r i (eval e1 r)) |
	Fun(i, a) -> FunVal(i, a, r) |
	FunCall(f, eArg) -> 
		(let fClosure = (eval f r) in
			(match fClosure with
				FunVal(arg, fBody, fDecEnv) -> 
					eval fBody (bind fDecEnv arg (eval eArg r)) |
				RecFunVal(g, (arg, fBody, fDecEnv)) -> 
					let aVal = (eval eArg r) in
						let rEnv = (bind fDecEnv g fClosure) in
							let aEnv = (bind rEnv arg aVal) in
								eval fBody aEnv |
				_ -> failwith("non functional value"))) |
	Letrec(f, funDef, letBody) ->
			(match funDef with
				Fun(i, fBody) -> (let r1 = (bind r f (RecFunVal(f, (i, fBody, r)))) in
												eval letBody r1) |
				_ -> failwith("non functional def")) |
	Select(i,et) ->  (match et with
						ETree(Empty) -> Tree(Empty) |
						ETree(Node(idn,en,t1,t2)) -> if i = idn then eval et r 
													 else let s1 = eval (Select(i,ETree(t1))) r in
														 	(match s1 with
														 		Tree(Empty) -> eval (Select(i,ETree(t2))) r |
																_ -> s1)  |
						_ -> failwith("non tree def")) |
	ApplyOver(tags,f,et) -> (match (tags,et) with
								([],ETree(_)) -> eval et r | (* se elenco  di tags è vuoto la funzione non deve esswre applicata a nessun nodo*)
								(x::xs,ETree(Empty)) -> Tree(Empty) | 
								(x::xs,ETree(Node(idn,en,t1,t2))) -> let ris1 = eval (ApplyOver(tags, f, ETree(t1))) r in
							   								 		 let ris2 = eval (ApplyOver(tags, f, ETree(t2))) r in
															 		 if List.exists (function el -> el = idn) tags then
																		Tree(Node(idn, eval (FunCall(f,en)) r, ris1, ris2)) (* sul nodo deve essere applicata f *)
															  		 else 	
															  			Tree(Node(idn, eval en r, ris1, ris2)) |
								_ -> failwith("non tree def"));;


(* =============================  TESTS  ================= *)

(* basico: no let *)
let env0 = emptyenv Unbound;;

let e1 = FunCall(Fun("y", Sum(Den "y", Eint 1)), Eint 3);;

eval e1 env0;;

let e2 = FunCall(Let("x", Eint 2, Fun("y", Sum(Den "y", Den "x"))), Eint 3);;

eval e2 env0;;

(* =============================  TESTS per alberi di espressioni  ================= *)
print_endline "*** TEST Alberi di espressioni ***";;
print_endline " Section 1 - Select(i,et)";;
(* 1 Test Select(i,et) *)
let t1 = ETree(
			Node("a",Sum(Eint 2, Eint 2), 
				Node("b",Prod(Eint 1,Eint 3),  
					Node("d",Fun("y", Sum(Den "y", Eint 1)),
						Empty,
						Empty
						),
					Node("d",Eint(5),
						Empty,
						Empty
						)
					),
				Node("b",Minus(Eint 2),
					Empty,
					Node("a", 
						Eint(6),
						Empty,
						Empty
						)
					)
				)
			);;
(* 1.1 selezione nodo "a" presente nella radice e in un altro nodo dell'albero. 
Deve essere selezionato il nodo nella radice in quanto la visita dell'albero procede con una visita anticipata*)
print_string "  1.1: ";;
match eval (Select("a", t1)) env0 with 
		Tree
			(Node ("a", Int 4,
			Tree
				(Node ("b", Int 3,
				Tree
				(Node ("d", FunVal ("y", Sum (Den "y", Eint 1), env0), Tree Empty,
					Tree Empty)),
				Tree (Node ("d", Int 5, Tree Empty, Tree Empty)))),
			Tree
				(Node ("b", Int (-2), Tree Empty,
				Tree (Node ("a", Int 6, Tree Empty, Tree Empty)))))) -> print_endline "Passed" |
		_ -> print_endline "Failed";;
(* 1.2 selezione nodo "b" presente anche nell'altro nodo allo stesso livello.
Deve essere selezionato il sottoalbero sinistro in quanto la visita dell'albero procede con una visita anticipata*)
print_string "  1.2: ";;
match eval (Select("b", t1)) env0 with 
		Tree
		(Node ("b", Int 3,
		Tree
			(Node ("d", FunVal ("y", Sum (Den "y", Eint 1), ev0), Tree Empty,
			Tree Empty)),
		Tree (Node ("d", Int 5, Tree Empty, Tree Empty)))) -> print_endline "Passed" |
		_ -> print_endline "Failed";;
(* 1.3 selezione nodo "c" non presente nell'albero.
Deve restituire un albero vuoto *)
print_string "  1.3: ";;
match eval (Select("c", t1)) env0 with 
		Tree(Empty) -> print_endline "Passed" |
		_ -> print_endline "Failed";;

print_endline " Section 2 - ApplyOver(tags,f,et)";;
(* 2 Test ApplyOver(tags,f,et) *)
let t2 = ETree(
			Node("a",Sum(Eint 2, Eint 2), 
				Node("b",Prod(Eint 1,Eint 3),  
					Node("d",Eint 6,
						Empty,
						Empty
						),
					Node("d",Eint(5),
						Empty,
						Empty
						)
					),
				Node("b",Minus(Eint 2),
					Empty,
					Node("a", 
						Minus(Eint 5),
						Empty,
						Empty
						)
					)
				)
			);;
let somma10 = Fun("x", Sum(Den "x", Eint 10));;
(* 2.1 applicazione di funzione somma10 su nodi con tag "a"
I nodi con tag uguale ad "a" devono essere applicati alla funzione somma10 ed ognuono di 
essi dovrà contenere il risultato della valutazione della funzione *)
print_string "  2.1: ";;
match eval (ApplyOver(["a"], somma10 ,t2)) env0 with 
		Tree
		(Node ("a", Int 14,
		Tree
			(Node ("b", Int 3, Tree (Node ("d", Int 6, Tree Empty, Tree Empty)),
			Tree (Node ("d", Int 5, Tree Empty, Tree Empty)))),
		Tree
			(Node ("b", Int (-2), Tree Empty,
			Tree (Node ("a", Int 5, Tree Empty, Tree Empty)))))) -> print_endline "Passed" |
		_ -> print_endline "Failed";;

(* 2.2 applicazione di funzione somma10 su nodi con tag "z" tag non presente nell'albero)
La funzione somma10 non deve essere applicata a nessun nodo dell'albero *)
print_string "  2.2: ";;
match eval (ApplyOver(["z"], somma10 ,t2)) env0 with 
		Tree
		(Node ("a", Int 4,
		Tree
			(Node ("b", Int 3, Tree (Node ("d", Int 6, Tree Empty, Tree Empty)),
			Tree (Node ("d", Int 5, Tree Empty, Tree Empty)))),
		Tree
			(Node ("b", Int (-2), Tree Empty,
			Tree (Node ("a", Int (-5), Tree Empty, Tree Empty)))))) -> print_endline "Passed" |
		_ -> print_endline "Failed";;

(*  2.3 applicazione di funzione somma10 su nodi con tag "a" oppure "b".
La funzione somma10 deve essere applicata ai soli nodi con tag "a" oppure "b" *)
print_string "  2.3: ";;
match eval (ApplyOver(["a";"b"], somma10 ,t2)) env0 with 
		Tree
		(Node ("a", Int 14,
		Tree
			(Node ("b", Int 13, Tree (Node ("d", Int 6, Tree Empty, Tree Empty)),
			Tree (Node ("d", Int 5, Tree Empty, Tree Empty)))),
		Tree
			(Node ("b", Int (8), Tree Empty,
			Tree (Node ("a", Int (5), Tree Empty, Tree Empty)))))) -> print_endline "Passed" |
		_ -> print_endline "Failed";;