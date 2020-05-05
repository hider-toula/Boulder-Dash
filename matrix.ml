(***********************)
(* Projet 2I008, 18/19 *)
(***********************)

(** Ce fichier contient une structure de matrice implanté à l'aide de
    maps. *)
  

(* ----------------------------Définition de type------------------------ *)
type 'a t =
  {
    larg    : int;
    haut    : int;
    map     : ('a Assoc.t) Assoc.t;
  }

(*  --------------------définition de l'exception Out_of_bounds--------------- *)

exception Out_of_bounds 

(* ################################################################## *)
(* à ne pas toucher *)
let pp
    (pp1: Format.formatter -> 'a -> unit)
    (fmt: Format.formatter)
    (m  : 'a t): unit =
  Format.fprintf fmt "@[<v 2>{@,larg:%d@,haut:%d@,m:%a}}@]"
    m.larg
    m.haut
    (Assoc.pp (Assoc.pp pp1)) m.map

(* ################################################################## *)


(* ------------------définition de la fonction make-------------------  *)


let make (haut: int) (larg: int) (default: 'a): 'a t =
  {
    larg=larg ;
    haut=haut ;
    map=Assoc.constant(Assoc.constant default);
  }


(* -----------------définitionde la fonction read------------------------ *)

let read (i: int) (j: int) (m: 'a t): 'a =
  Assoc.find j (Assoc.find i m.map) 


(* ----------------définition de la fonction set-------------------------- *)


let set ( i: int) ( j: int) ( v: 'a) ( m: 'a t): 'a t =
  {m with map=Assoc.set i (Assoc.set j v (Assoc.find i m.map) ) m.map }

(* -----------------définition de la fonction fold------------------------- *)

let fold (f: int -> int -> 'a -> 'b -> 'b) (m: 'a t) (acc: 'b): 'b = 

   let rec loop i j res =
      if (j=m.larg-1) then 
                        (
                          if (i=m.haut-1) then (f i j ( read i j m ) res) 
                          else loop (i+1) 0 (f i j  (read i j m ) res) 
                          
                        )
      else loop i (j+1) (f i j  (read i j m ) res)

 in loop 0 0 acc   

 (* --------------------définition de la fonction iter----------------------  *)


let iter (f: int -> int -> 'a -> unit) (m: 'a t): unit =
  let g x y m b =
    f x y m ; b 
    in 
  fold g m ();