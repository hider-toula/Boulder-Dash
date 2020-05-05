open Boulder_dash
open Graphics
open Game
open Type



let message color message  =
  set_color color;
  moveto (size_x()/2) (size_y()/2) ;
  set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  draw_string message 

let handle ( key: (Graphics.status)) (g: game) : game =
  let g= world_turn g in 
  if (win g) then raise Win
  else if key.key ='e' then world_turn ( player_turn g N )
  else if key.key ='d' then world_turn ( player_turn g S )
  else if key.key= 's' then world_turn ( player_turn g W )
  else if key.key ='f' then world_turn ( player_turn g E )
  else if key.key = 'x' then exit(1)
  else world_turn g



let rec turn g =
  let st = wait_next_event [Key_pressed] in
  let g = handle st g in
  let scale = Drawing.compute_scaler g in
  Drawing.reinit_graphics ();
  let () = Drawing.draw_game g scale in
  

  turn g




let game ()  =
  let game = Parse.parse_file "data/level0.lv" in
  Drawing.init_graphics ();
  let scale = Drawing.compute_scaler game in
  Drawing.reinit_graphics ();
  let () = Drawing.draw_game game scale in
  try
    turn game
  with  |Win  ->  (moveto (size_x()/4) (size_y()/2) ; set_color green ; draw_string "FELICITATION!" );synchronize() ;Unix.sleep 3
        |Dead   -> (moveto (size_x()/4) (size_y()/2) ; set_color red ; draw_string "    PERDU!!" );synchronize() ;Unix.sleep 3
let () =
  game ()
