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
#use "tp2.mli";;

open List;;

module Tp2 : TP2 = struct

    (* Utilisée par le testeur et correcteur du Tp                             *)
    exception Non_Implante of string


(* Principales structures de données du TP ------------------------------- *)
type action = Epsilon | Api of string
type etat = int
type transition = etat * action * etat
type programme = transition list * etat

(* Signatures des fonctions du Tp à implanter --------------------------- -*)
  (* ----------------------------------------------------------------------- *)


(* ----------------------------------------------------------------------- *)
(* Partie réservée aux fonctions utiles ---------------------------------- *)
(* ----------------------------------------------------------------------- *)
let rec elements_unique liste_a_traiter =
  match liste_a_traiter with
  | [] -> []
  | element :: reste -> 
    if mem element reste then elements_unique reste
  else element :: elements_unique reste;;

let etats pgm = snd pgm :: 
    (fst pgm
    |> map (fun (etat1, _, etat2) -> [etat1; etat2])
    |> concat)
    |> elements_unique;;
  
;;

  (* ----------------------------------------------------------------------- *)
  (* Début partie code (implantation) à compléter -------------------------- *)
  (* ----------------------------------------------------------------------- *)

let transitionsImmediates pgm etat = 
  (fst pgm |> filter(fun (a,b,c) -> a == etat && b != Epsilon),
  fst pgm |> filter(fun (a,b,c) -> a == etat && b == Epsilon))
;;

let epsilonAtteignable pgm = let rec recherche liste_a_traiter liste_resultats = match liste_a_traiter with
| [] -> liste_resultats
| etat :: reste -> let prochains_etats =
snd (transitionsImmediates pgm etat)
    |> filter (fun (etat_prec, _, etat_succ) -> (not (mem etat_succ liste_resultats)) && (not (mem etat_succ reste)))
    |> map (fun (_, _, etat_succ) -> etat_succ)
    |> elements_unique
  in recherche (append reste prochains_etats) (append liste_resultats prochains_etats)

in etats pgm |> map(fun etat -> (etat, (recherche [etat] [])))
    
;;
let supprimeEpsilon pgm = let etats_P' = append (fst pgm |> filter(fun (_, action, _) -> action != Epsilon )|> map (fun (_, _, etat') -> etat')) [snd pgm] |> elements_unique
in ((etats_P' |> map (fun etat -> fst (transitionsImmediates pgm etat)) |> concat) @
(epsilonAtteignable pgm 
  |> map (fun (etat, liste) -> if mem etat etats_P'
    then liste |> map (fun etat' -> fst (transitionsImmediates pgm etat')) |> concat |> map (fun (_, a, b) -> (etat, a, b)) else []) |> concat)
|> elements_unique, snd pgm)

;;
let similaire pgm1 pgm2 = let rec execute_prog pgm1' pgm2' deja_vu = let transition2 = fst (transitionsImmediates pgm2' (snd pgm2'))
in match pgm1' with
| ([], _) -> true
| ((_, Epsilon, _) :: reste, etat) -> execute_prog (reste, etat) pgm2' deja_vu
| ((etat, action, etat_succ) :: reste, etat_init) -> if etat=etat_init 
then
  transition2 |> exists (fun (_, action', _) -> action = action') &&
    transition2 |> for_all (fun (_, action', etat_succ') -> if action' = action 
    then 
      (execute_prog (reste, etat_succ) (fst pgm2', etat_succ') []) && (execute_prog (reste, etat) pgm2' [])
    else true)
else if mem (etat, action, etat_succ) deja_vu 
  then true
  else execute_prog (reste@[(etat, action, etat_succ)], etat_init) pgm2' ((etat, action, etat_succ)::deja_vu)
in execute_prog pgm1 pgm2 []

;;

let bisimilaire pgm1 pgm2 =
    (similaire pgm1 pgm2) && (similaire pgm2 pgm1)
;;
let estSousPgm pgm1 pgm2 = etats pgm2 |> exists (fun etat -> similaire pgm1 (fst pgm2, etat))
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

end;;