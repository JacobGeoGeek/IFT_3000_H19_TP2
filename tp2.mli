(*****************************************************************************) 
(* TP2 Hiver 2019 - IFT-3000                                                 *) 
(*****************************************************************************) 

(*****************************************************************************) 
(* Spécification                                                             *) 
(*****************************************************************************) 
module type TP2 = sig

  (* Principaux types du Tp ------------------------------------------------ *)
  (* ----------------------------------------------------------------------- *)
  type  action = Epsilon | Api of string
  type  etat = int

  type  transition = etat * action * etat
  type programme = transition list * etat
  
       
      
  (* Signatures des fonctions du Tp à implanter --------------------------- -*)
  (* ----------------------------------------------------------------------- *)

  (* 5 points *)
  val transitionsImmediates : 
    programme -> etat -> transition list * transition list

  (* 30 points *)
  val epsilonAtteignable : programme -> (etat * etat list) list

  (* 30 points *)
  val supprimeEpsilon : programme -> programme

  (* 20 points *)
  val similaire : programme -> programme -> bool

  (* 5 points *)
  val bisimilaire : programme -> programme -> bool

  (* 10 points *)
  val estSousPgm : programme -> programme -> bool
end

(* Exemples utilisés dans l'énoncé:*)
(*
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
*)

