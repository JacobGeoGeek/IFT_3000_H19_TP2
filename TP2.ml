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

module Tp : TP = struct

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


  (* ----------------------------------------------------------------------- *)
  (* Début partie code (implantation) à compléter -------------------------- *)
  (* ----------------------------------------------------------------------- *)


let transitionsImmediates pgm etat =
  let (transi,etatInit) = pgm in
    (List.filter(fun (a,b,c) -> a == etat && b == Epsilon) transi), (List.filter(fun (a,b,c) -> a == etat && b != Epsilon ) transi)



let epsilonAtteignable pgm =
    raise (Non_Implante "esilonAtteignable à compléter")

let supprimeEpsilon pgm =
    raise (Non_Implante "supprimeEpsilon à compléter")

let similaire pgm1 pgm2 =
  raise (Non_Implante "supprimeEpsilon à compléter")

let bisimilaire pgm1 pgm2 =
    (similaire pgm1 pgm2) && (similaire pgm2 pgm1)

let estSousPgm pgm1 pgm2 =
  raise (Non_Implante "estSousPgm à compléter")
    
end