open Type
open Structures
open Matrix

exception Dead 
exception Bould of (int*int) 
exception Win 

(* -----------------définition de position_afgter move------------------  *)

let position_after_move ( x,y : int*int ) (d : dir) : (int*int) =
  match d with 
  |N->(x+1,y)
  |S->(x-1,y)
  |W->(x,y-1)
  |E->(x,y+1)

let move (_, _) (_: dir) = assert false

(* ------------------définition de la fonction win--------------------------  *)

let win (g : game) : bool =
  g.diamond = 0 

  (* ----------définition de la fonction compte_diamonds-------------------- *)

  let count_diamonds (c : cell Matrix.t ) : int = 

    let counter (x:int) (y:int) (cel : cell)  (nb : int ) =
      if x=0 && y=0 then () ;
      match cel with 
        |Diamonts-> nb+1 
        |Walnut-> nb+1
        |_->nb
      in 
    Matrix.fold counter   c 0

(* --------------définition de la fonction player_turn-------------------- *)

let player_turn (gm: game) (dr: dir) = 
  if ( not (win gm )) then (
  let (x,y)= gm.player in 
  let (i,j)= position_after_move gm.player dr in 
  match (Matrix.read i j gm.map) with 
  |Empty->{gm with player=(i,j); map=Matrix.set x y Empty gm.map  }
  |Dirt->   { gm with map = Matrix.set i j Empty  gm.map ; player =(i,j)}
  |Diamonts-> {  map = Matrix.set i j Empty  gm.map ; player =(i,j) ; diamond = gm.diamond-1 }
  |Stone-> gm
  |Door->raise Win
  |Walnut->gm
  |Boulder ->( match dr with 
                |W|E-> (let (posx,posy)= position_after_move (i,j) dr in
                         match Matrix.read posx posy gm.map with 
                               |Empty-> let (z,t)=gm.player in {gm with map=Matrix.set z t Empty (Matrix.set posx posy Boulder gm.map); player = (i,j) }  
                               |_->gm)
              |_->gm)
    
  )
  else raise Win 
               
(* --------------------definition de is_empty---------------------------- *)

let is_empty ((x,y) : (int*int)) (g :game ) : bool = 
  match read x y g.map with
  |Empty->true
  |_->false

(* ---------définition de la fonction position after_fall---------------- *)

let position_after_fall (g : game) ((i,j):(int*int))  : (int*int) =
  if is_empty (i-1,j) g then (i-1,j)
  else if is_empty (i-1,j-1) g && is_empty (i,j-1) g && (read (i-1) j g.map ) = Boulder then (i-1,j-1)
  else if is_empty (i-1,j+1) g && is_empty (i,j+1) g && (read (i-1) j g.map ) = Boulder then (i-1,j+1)
  else (i,j)

(* --------------définition de move_boulder_step----------------------  *)

let move_boulder_step (g : game) ((i,j) : (int*int)) : game = 
  let (x,y)=position_after_fall g (i,j) in 
  if (x,y)=g.player then  raise Dead 
  else if  ((read (x-1) y g.map )= Walnut)  then {g with map=Matrix.set i j Empty (Matrix.set  x y Boulder (Matrix.set (x-1) y Diamonts g.map)) ; diamond = g.diamond+1  }
  else {g with map=Matrix.set i j Empty (Matrix.set  x y Boulder  g.map) ;  }

(* -----------définition de la fonction find_mouvable_boulder----------- *)

  let find_mouvable_boulder (g : game) : (int*int) option =
    let fonction_temp x y elemt = 
      if elemt !=Boulder then ()
      else if position_after_fall g (x,y) = (x,y) then ()
      else raise (Bould (x,y)) 
    in 
    try
      Matrix.iter fonction_temp g.map ; None 
    with Bould (x,y) -> Some(x,y)

(* ----------------définir nla fonction world_turn ----------------------- *)
  let world_turn (g : game) : game = 
    let rec loop g =
      match find_mouvable_boulder g with
        |None-> g
        |Some  p->Unix.sleepf 0.1 ;loop (move_boulder_step g p) 

    in loop g


      