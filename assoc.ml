(***********************)
(* Projet 2I008, 18/19 *)
(***********************)

(** Ce fichier contient une structure de map (int -> 'a), les paires
   clé/valeur sont stockés dans une liste ordonnée sur les clés. *)

(* ----------------------définition de remove_assoc-------------------------- *)


let remove_assoc (k: int) (l: (int * 'a) list): (int * 'a) list =
  let rec loop l accu =
    match l with 
    |[]->List.rev accu
    |x::xs-> let (n,_)= x in if (k=n) then loop xs accu 
                            else 
                            (
                              loop xs (x::accu)
                            )
    in loop l []

(* ######################################################################## *)
(* à ne pas toucher *)
let pp_assoc
    (pp1: Format.formatter -> 'a -> unit)
    (fmt: Format.formatter)
    (l: (int * 'a) list): unit =
  Format.fprintf fmt "@[[%a@]]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
       (fun fmt (k, v) -> Format.fprintf fmt "(%a, %a)" Format.pp_print_int k pp1 v)
    ) l

(* Définition du type des fonctions des entiers vers 'a *)
type 'a t =
  { assoc : (int * 'a) list;
    default : 'a
  }

  (* ######################################################################### *)


(* -------------------------définition de la fonction constant----------------- *)

let constant ( d: 'a): 'a t =
  { assoc=[];default=d } 

(* ---------------------------definition de la fonction find------------------- *)


let find (k: int) (m: 'a t): 'a =
  try
    List.assoc k m.assoc
  with Not_found -> m.default

(* ---------------------------définition de la fonction set------------------- *)


let set (k: int) (v: 'a) (m: 'a t): 'a t =
  if v=m.default then {m with assoc = remove_assoc k m.assoc } 
  else
      (
        let rec loop   acc =
          match acc with
          |[]->[(k,v)]
          |(x,y)::xs->if (x==k) then (x,v)::xs 
                      else if k<x then ((k,v)::(x,y)::xs )
                      else  (x,y)::loop xs
      
        in {m with assoc=loop m.assoc }
      )



(* -------------------------------définition de la fonction fold--------------- *)

let fold (f: int -> 'a -> 'b -> 'b) (a: int) (b: int) (m: 'a t) (init: 'b): 'b =
  let rec loop a accu = 
    if a<b then  loop (a+1) (f a (find a m) accu) 
    else  f a (find a m )  accu 
    in  loop a init 


(* ########################################################################### *)

(* à ne pas toucher *)
let pp
    (pp1: Format.formatter -> 'a -> unit)
    (fmt: Format.formatter)
    (m: 'a t): unit =
  Format.fprintf fmt "@[{default:%a;@, assoc:[@[%a@]]@,}@]"
    pp1 m.default
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@,")
       (fun fmt (k, v) -> Format.fprintf fmt "(%a, %a)" Format.pp_print_int k pp1 v)
    ) m.assoc

(* ########################################################################## *)