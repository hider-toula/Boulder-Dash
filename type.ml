(***********************)
(* Projet 2I008, 18/19 *)
(***********************)

open Structures

(** Ce fichier contient les définitions des types principaux du jeu *)

(* ------------------------definition de cel-------------------------- *)

type cell = Empty|Stone|Boulder|Dirt|Diamonts|Walnut|Door

(* -------------définition de la fonction string of celle--------------- *)


let string_of_cell c =
  match c with 
  |Empty->"vider"
  |Stone->"stone"
  |Boulder->"boulder"
  |Dirt->"dirt"
  |Diamonts->"diamants"
  |Walnut->"noix"
  |Door->"porte"

let pp_cell fmt cell = Format.pp_print_string fmt (string_of_cell cell)

type map =
  cell Matrix.t

let pp_map : Format.formatter -> map -> unit = Matrix.pp pp_cell

(* ------------------définition du type game---------------------------- *)

type game =
  { map : map ; player : (int*int) ; diamond : int }

let print_game (fmt: Format.formatter) (g: game): unit =
  Format.fprintf fmt "@[<v>@[<v 2>{@,map: %a@,player: (%d, %d)@,}@]@,@]"
    pp_map g.map
    (fst g.player) (snd g.player)

  (* ------------------définition du type dir----------------------- *)

type dir = N|S|E|W

(* ------------définition de la fonction string_of_dir-------------- *)

let string_of_dir (d: dir) =
  match d with  
  |N->"nord"
  |S->"sud"
  |E->"est"
  |W->"west"


let pp_dir fmt dir = Format.pp_print_string fmt (string_of_dir dir)
