(*****************************************************************************) 
(* TP Hiver 2019 - IFT-3000                                                  *) 
(*                                                                           *) 
(* Vérification automatique de programmes.                                   *) 
(**************************************************************************  *) 
(* NOM: Chaar             PRÉNOM: Jacob      *) 
(* MATRICULE: 111 180 596             PROGRAMME: IFT         *) 
(*NOM: Rochon             PRÉNOM: Louis      *)
(*MATRICULE: 111 179 234               PROGRAMME: IFT*)
(**************************************************************************  *) 


(*module Tp : TP = struct*)

    (* Utilisée par le testeur et correcteur du Tp                             *)
    exception Non_Implante of string


(* Principales structures de données du TP ------------------------------- *)
type programme = transition list * etat
and  transition = etat * action * etat
and  action = Epsilon | Api of string
and  etat = int

(* Signatures des fonctions du Tp à implanter --------------------------- -*)
  (* ----------------------------------------------------------------------- *)





(* ----------------------------------------------------------------------- *)
(* Partie réservée aux fonctions utiles ---------------------------------- *)
(* ----------------------------------------------------------------------- *)

let rec lesSucesseur prog etat output =
  match prog with
  | [] -> output
  | hd::tl -> let (a,b,c) = hd in
    if a==etat && b == Epsilon && (List.mem c output)==false
    then lesSucesseur tl etat (output @ (c::[]))
    else lesSucesseur tl etat output
;;   

let rec lesEtatsSucc pgm fil parcourt output= 
  match fil with
  | [] -> output
  | hd::tl -> 
  if (List.mem hd parcourt) = false then
      lesEtatsSucc pgm (tl @ (lesSucesseur pgm hd [])) ([hd] @ parcourt) (output @ (lesSucesseur pgm hd []))
  else lesEtatsSucc pgm tl parcourt output
  
;;

  (* ----------------------------------------------------------------------- *)
  (* Début partie code (implantation) à compléter -------------------------- *)
  (* ----------------------------------------------------------------------- *)


let transitionsImmediates pgm etat =
  let (transi,etatInit) = pgm in
    (List.filter(fun (a,b,c) -> a == etat && b != Epsilon) transi), (List.filter(fun (a,b,c) -> a == etat && b == Epsilon ) transi)
;;



let epsilonAtteignable pgm =
    let (transi,etatInit) = pgm in
    List.map(fun (a,b,c) -> (a, (lesEtatsSucc transi [a] [] []))) transi
;;
let supprimeEpsilon pgm =
    raise (Non_Implante "supprimeEpsilon à compléter")
;;
let similaire pgm1 pgm2 =
  raise (Non_Implante "supprimeEpsilon à compléter")

let bisimilaire pgm1 pgm2 =
    (similaire pgm1 pgm2) && (similaire pgm2 pgm1)
;;
let estSousPgm pgm1 pgm2 =
  raise (Non_Implante "estSousPgm à compléter")
;;

let pgm1 = 
	([(1, Api "a", 2); (2, Epsilon, 6); (2, Epsilon, 6); (6, Api "e", 7);
	  (7, Epsilon, 7); (7, Api "exit", 10)],1);;

let pgm2 = 
	([(1, Api "a", 2); (2, Epsilon, 5); (5, Epsilon, 6); (6, Api "c", 7);
	  (7, Epsilon, 7); (1, Epsilon, 3); (3, Api "a", 4); (4, Epsilon, 4);
	  (4, Api "b", 8)],1);;

let pgm3 = 
	([(1, Epsilon, 2); (2, Api "a", 6); (6, Epsilon, 7); (7, Api "b", 10);
      (10, Epsilon, 10); (6, Api "c", 8); (1, Epsilon, 4); 
      (4, Epsilon, 1)], 1);;

let pgm4 = 
	([(0, Api "a", 6); (0, Epsilon, 3); (6, Api "b", 10); (6, Api "c", 8);
	  (8, Epsilon, 8)],0);;


transitionsImmediates pgm1 1;;
transitionsImmediates pgm3 1;;
transitionsImmediates pgm1 2;;
transitionsImmediates pgm3 6;;
transitionsImmediates pgm1 7;;
transitionsImmediates pgm1 10;;

epsilonAtteignable pgm1;;
epsilonAtteignable pgm2;;
epsilonAtteignable pgm3;;


let pgm1' = supprimeEpsilon pgm1;;
let pgm2' = supprimeEpsilon pgm2;;
let pgm3' = supprimeEpsilon pgm3;;
supprimeEpsilon pgm4;;

similaire pgm2' pgm3';;
similaire pgm3' pgm2';;

bisimilaire pgm2' pgm3';;
bisimilaire pgm3' pgm4;;
bisimilaire pgm4 pgm3';;

estSousPgm ([(0, Api "e", 1); (1, Api "exit", 2)],0) pgm1;;
estSousPgm ([(0, Api "a", 1); (1, Api "c", 2)],0) pgm3;;
estSousPgm pgm1' pgm1';;
List.map (estSousPgm ([],0)) [pgm1;pgm2;pgm3;pgm4;pgm1';pgm2';pgm3'];;