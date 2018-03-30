type  terme = X of int | F1 of terme | F2 of terme | F3 of terme
              | T | G1 of terme * terme | G2 of terme * terme
              | H1 of terme * terme * terme
              | H2 of terme * terme * terme
              | A1
              | A2
              | A3;;


let rec concat_2 l1 l2 =
	match l1 with
	| [] -> l2
	| x1::ll1 -> x1::(concat_2 ll1 l2)
;;


let concat_3 l1 l2 l3 =  concat_2 (concat_2 l1 l2) l3 ;;

(* Test :

# concat_2 [3;3] [2;1];;
- : int list = [3; 3; 2; 1]


# concat_3 [2;1] [9;5] [6;7]
  ;;
- : int list = [2; 1; 9; 5; 6; 7]


Les fonctions de concaténations marchent parfaitement à mes yeux.
*)


# si deux elements sont égaux, on crée []

let rec regle terme_1 terme_2 =
  match (terme_1, terme_2) with
  |  (terme_1, terme_2) when terme_1 = terme_2    -> []
  |  (a, X(x))                                    -> [(X(x),a)]
  |  (X(x), a)                                    -> [(X(x),a)]
  |  (F1(m), F1(z))                               -> regle m z
  |  (F2(m), F2(z))                               -> regle m z
  |  (F3(m), F3(z))                               -> regle m z
  |  (G1(m1, m2),G1(z1,z2))                       -> concat_2 (regle m1 z1) (regle m2 z2)
  |  (G2(m1, m2),G2(z1, z2))                      -> concat_2 (regle m1 z1) (regle m2 z2)
  |  (H1(m1, m2, m3),H1(z1, z2, z3))              -> concat_3 (regle m1 z1) (regle m2 z2) (regle m3 z3)
  |  (H2(m1, m2, m3),H2(z1, z2, z3))              -> concat_3 (regle m1 z1) (regle m2 z2) (regle m3 z3)
  |  _                                            -> [(T,T)]
;;



(* Test :


#  regle (A1) (A1);;
- : (terme * terme) list = []

# regle (A1) (X(1));;
- : (terme * terme) list = [(X 1, A1)]

# regle (X(1)) A1 ;;
- : (terme * terme) list = [(X 1, A1)]

# regle (F1(A2)) (F1(A1));;
- : (terme * terme) list = [(T, T)]

# regle (F1(A2)) (F1(A2));;
- : (terme * terme) list = []

# regle (F1(X(1))) (F1(X(3)));;
- : (terme * terme) list = [(X 3, X 1)]

regle ( H1(X(1), X(2), A2) ) ( H1(X(5), A1, X(7)) );;
- : (terme * terme) list = [(X 5, X 1); (X 2, A1); (X 7, A2)]

regle ( H1(X(1), F1(X(2)), A2) ) ( H1(X(5), A1, X(7)) );;
- : (terme * terme) list = [(X 5, X 1); (T, T); (X 7, A2)]

regle ( H1(X(1), F1(X(2)), A2) ) ( H1(X(5), F1(A1), X(7)) );;
- : (terme * terme) list = [(X 5, X 1); (X 2, A1); (X 7, A2)]


La fonction marche parfaitement à mes yeux.

*)


let rec presence a mylist =
  match mylist with
  | []          -> false
  | (x1,x2)::q  -> let (a1,a2) = a in
                    if x1==a1 || x2==a2 then true else presence a q
;;


(* Test :

presence (T,T) [ (X(1),A1) ; (X(2),A3) ; (T,T) ];;
- : bool = true

presence (T,T) [ (X(1),A1) ; (X(2),A3)];;
- : bool = false

presence (T,T) [];;
- : bool = false

La fonction marche parfaitement à mes yeux.

*)

let rec remplace terme (c1,c2) =
    match terme with
    | X(x) when (X(x)=c1)  -> c2
    | F1(x)                 -> F1(remplace x (c1,c2))
    | F2(x)                 -> F2(remplace x (c1,c2))
    | F3(x)                 -> F3(remplace x (c1,c2))
    | G1(x1,x2)             -> G1( remplace x1 (c1,c2), remplace x2 (c1,c2) )
    | G2(x1,x2)             -> G2( remplace x1 (c1,c2), remplace x2 (c1,c2) )
    | H1(x1,x2,x3)          -> H1( remplace x1 (c1,c2), remplace x2 (c1,c2), remplace x3 (c1,c2) )
    | H2(x1,x2,x3)          -> H2( remplace x1 (c1,c2), remplace x2 (c1,c2), remplace x3 (c1,c2) )
    | _                     -> terme
;;

(* Test :

#remplace (X(1)) (X 1,A1);;
- : terme = A1

#remplace (X(1)) (X(3),A1);;
- : terme = X 1

remplace (X(1)) (X(1),A1);;
- : terme = A1

remplace (G2( X(2), F1(X(3)) )) (X(3),A2);;
- : terme = G2 (X 2, F1 A2)

remplace A2 (X(6),A2);;
- : terme = A2

La fonction marche parfatement à mes yeux.
*)


let rec substitution terme remplacement =
  match remplacement with
  | [] -> terme
  | (x1,x2)::q ->  substitution (remplace terme (x1,x2) ) q
;;

(* Test :


substitution (H1(X(1), X(2),A3)) [(X(1), A1); (X(2), A2)]  ;;
- : terme = H1 (A1, A2, A3)


substitution (H1(F1(X(1)), X(2),A3)) [(X(1), A1); (X(2), A2)]  ;;
- : terme = H1 (F1 A1, A2, A3)

La fonction marche parfaitement à mes yeux.

*)


let rec unification terme_1 terme_2 =
  match regle terme_1 terme_2 with
  | []         -> (terme_1, terme_2)
  | tab_couple -> let test_unification = regle (substitution terme_1 tab_couple) (substitution terme_2 tab_couple) in
                  if (presence (T,T) test_unification) then (T,T)
                  else unification (substitution terme_1 tab_couple) (substitution terme_2 tab_couple)
;;



(* Test :

unification ( G1( X(1),X(2)) ) ( G1(A1,A2) );;
- : terme * terme = (G1 (A1, A2), G1 (A1, A2))

unification ( G1( F1(X(1)),X(2)) ) ( G1(A1,A2) );;
- : terme * terme = (T, T)

unification ( G1( F1(X(1)),X(2)) ) ( G1(F1(A1),A2) );;
- : terme * terme = (G1 (F1 A1, A2), G1 (F1 A1, A2))

unification ( G1( F1(X(1)),X(2)) ) ( G1(F1(A1),A2) );;
- : terme * terme = (G1 (F1 A1, A2), G1 (F1 A1, A2))

unification ( G1( F1(X(1)),X(3)) ) ( G1(F1(X(3)),A2) );;
- : terme * terme = (G1 (F1 A2, A2), G1 (F1 A2, A2))



La foncton marche parfatement à mes yeux.
*)
