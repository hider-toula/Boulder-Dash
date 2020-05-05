(***********************)
(* Projet 2I008, 18/19 *)
(***********************)

(** Ce fichier contient les fonctions d'affichage graphique du jeu *)

open Graphics
open Type
open Structures
open Matrix

type scale =(int*int) -> (int*int)

let gray = rgb 105 105 105
let brown = rgb 139 69 19
let diamond = rgb 185 242 255

let compute_scaler (g: game): scale =
  let sx = size_x () in
  let sy = size_y () in
  let rx = (sx - sx / 5) / g.map.larg in
  let ry = (sy - sy / 5) / g.map.haut in
  let sxi = rx * g.map.larg in
  let syi = ry * g.map.haut in
  let margx = (sx - sxi) / 2 in
  let margy = (sy - syi) / 2 in
  fun (x, y) ->
    (margx + x * rx, margy + y * ry)


(* ----------------définitioàn de la fonction draw_cell---------------------- *)

let draw_rect_cell (c: color) ((i, j): int * int) (scaler: scale): unit =
  set_color c;
  let (x,y) = scaler(i,j) in
  let (w,h)= scaler(i+1,j+1) in
  draw_rect x y h w 

(* -------------la définition de la fonction fill_rect_cell------------------ *)

let fill_rect_cell (c: color) ((i, j): int * int) (scaler: scale): unit =
  set_color(c) ;
  let (x,y) = scaler(i,j) in
  let (w,h)= scaler(i+1,j+1) in
  fill_rect x y    (w-x) (h-y) 

(* ------------------la définition fill_diamond_cell------------------------- *)

let fill_diamond_cell (c: color) ((i, j): int * int) (scaler: scale): unit =
  set_color c ;
  let (x,y) = scaler(i,j) in
  let (w,h)= scaler(i+1,j+1) in
  fill_poly  [|(x+(w-x)/2,y);(w,y+(h-y)/2);(x+(w-x)/2,h);(x,y+(h-y)/2)|]


(* ----------------définition de la fonction fill_circle------------------ *)

let fill_circle_cell (c: color) ((i, j): int * int) (scaler: scale): unit =
  set_color c ;
  let (x,y) = scaler(i,j) in
  let (w,h)= scaler(i+1,j+1) in
  fill_circle (x+(w-x)/2) (y+(h-y)/2) ((w-x)/2)

(* --------------définition de la fonction draw_fill_noix---------------- *)

let fill_walnut_cell (c : color) ((i,j) : int*int ) (scaler : scale) : unit =

  set_color c ;
  let (x,y) = scaler(i,j) in
  let (w,h)= scaler(i+1,j+1) in
  fill_ellipse (x+((w-x)/2)) (y+(h-y)/2) ((w-x)/4) ((w-x)/2)

(* --------------définition de la fonction draw_door----------------------- *)

let draw_door (c : color) ((i,j) : int*int ) (scaler : scale) : unit =
  fill_rect_cell red (i,j) scaler ; 
  fill_walnut_cell c (i,j) scaler 


(* -----------------definition de la fonction draw_cell--------------------- *)

let draw_cell (c: cell) (i, j) (scaler: scale): unit =
  match c with 
  |Empty->()
  |Stone->fill_rect_cell black (i,j) scaler
  |Boulder-> fill_circle_cell gray (i,j) scaler
  |Dirt->fill_rect_cell brown (i,j) scaler
  |Diamonts->fill_diamond_cell diamond (i,j) scaler
  |Walnut-> fill_walnut_cell brown (i,j) scaler 
  |Door->draw_door brown (i,j) scaler

(* ------------------définition de la fonction draw_map----------------------- *)

let draw_map (m: map) (scaler: scale): unit =
  for i =0 to m.haut-1 do
    for j =0 to m.larg -1 do 
      draw_cell (read i j m) (j,i) scaler 
    done
  done

  
(* -----------------définition de la fonction draw_player-------------------  *)

let draw_player ((i, j): (int * int)) (scaler: scale): unit =
  fill_circle_cell red (j,i) scaler ;
  synchronize()


(* -------------------définition de la fonction draw_game------------------ *)

let draw_game (g: game) (scaler: scale) =
  synchronize();
  draw_map g.map scaler ;
  draw_player g.player scaler ;
  let (x,y) = scaler (0,g.map.haut) in 
  moveto x y ;
  set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  draw_string "Le Reste Est : "; 
  draw_string (string_of_int (g.diamond));
  synchronize()





(* ----------------définition de la fonction init_graphics------------------------ *)

let init_graphics (): unit =
  open_graph "";
  resize_window 800 800;
  auto_synchronize false

(* ---------------définition de la fonction reinit_graphics-------------------- *)
let reinit_graphics (): unit = 
  clear_graph  () ; 

