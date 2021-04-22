type couleur = Rouge | Noir;;
type element = int;;
type 'a ab = Empty | Node of element * couleur * ag * ad;;

let est_vide a = (a = Empty);;

let rec est_dans x a =
	match a with
	| Empty -> false
	| Node (e, _, ag, _) when (compar_elt x e) = -1 -> est_dans x ag
	| Node (e, _, _, ad) when (compar_elt x e) = 1 -> est_dans x ad
	| _, _ -> true;;

let is_ab a = 
	(is_abr a) && 
	(total_nb_noir a) && 
	(rouge_consec a) && 
	(racine_noire a);;

let racine_noire =
	function
		| Empty -> true
		| Node (_, Noir, _, _) -> true
		| _ -> false;;

let rouges_consec a =
	let rec aux f =
		function
			| [] -> true
			| (Empty, _)::q -> aux q
			| (Node (_, Noir, ag, ad), _)::q -> aux(ag, Noir)::((ad, Noir)::q)
			| (Node (_, _, ag, ad), Noir)::q -> aux(ag, Rouge)::((ad, Rouge)::q)
			|_ -> false
	in aux [a, Noir];;

let total_nb_noir a =
	let rec f n=
		function
			| Empty -> n
			| Node (_, _, ag, _) -> f(n + 1) ag
	in 
		let nb_noir = (f 0 a)
		in
			let rec aux =
				function
					| [] -> true
					| (Empty, n)::q when n = nb_noir -> aux q
					| (Node(_, Noir, ag, ad), n)::q -> aux((ag, n + 1)::((ad, n + 1)::q))
					| (Node(_, Rouge, ag, ad), n)::q -> aux((ag,n)::((ad,n)::q))
					| _ -> false 
			in aux [a, 0];;

let compar_elt e1 e2 =
	if e1<e2
	then -1
	else if e1>e2
				then 1
				else 0;;
