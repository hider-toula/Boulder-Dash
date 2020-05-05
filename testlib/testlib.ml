exception Not_implemented

let wrap module_name main () =
  Format.printf "@[=== Starting tests on module %s ===@]@\n" module_name;
  main ();
  Format.printf "@[=== Ending tests on module %s ===@]@\n" module_name

let print_ok id =
  Format.printf "@[\027[1;38;5;2m[✓]\027[0m Test %d@\n@]" id

let exists_error = ref false

let not_implemented id name =
  exists_error := true;
  Format.printf "@[<v>\027[1;38;5;172m[?]\027[0m Test %d: %s not_implemented@,@]" id name

let print_ko id message =
  exists_error := true;
  Format.printf "@[\027[1;38;5;1m[✗]\027[0m Test %d failed: " id;
  message ();
  Format.printf "@\n@]"

let test_arit1 test_id s f ppa a ppb b =
  try
    let y = f a in
    if y = b then print_ok test_id
    else
      let message () =
        Format.printf
          "@[function %s@]@\n@[<2>arg: %a@]@\n@[<2>@[<2>res: %a@]@]@\n@[<2>should be: %a@]@\n@]"
          s ppa a ppb y ppb b
      in
      print_ko test_id message
  with
  | Not_implemented -> not_implemented test_id s

let test_arit2 test_id s f ppa1 a1 ppa2 a2 ppb b =
  try
    let y = f a1 a2 in
    if y = b then print_ok test_id
    else
      let message () =
        Format.printf
          "@[function %s@]@\n@[<2>arg1: %a@]@\n@[<2>arg2: %a@]@\n@[<2>res: %a@]@\n@[<2>should be %a@]"
          s ppa1 a1 ppa2 a2 ppb y ppb b
      in
      print_ko test_id message
  with
  | Not_implemented -> not_implemented test_id s

let test_arit3 test_id s f ppa1 a1 ppa2 a2 ppa3 a3 ppb b =
  try
    let y = f a1 a2 a3 in
    if y = b then print_ok test_id
    else
      let message () =
        Format.printf
          "@[function %s@]@\n@[<2>arg1: %a@]@\n@[<2>arg2: %a@]@\n@[<2>arg3: %a@]@\n@[<2>res: %a@]@\n@[<2>should be %a@]"
          s ppa1 a1 ppa2 a2 ppa3 a3 ppb y ppb b
      in
      print_ko test_id message
  with
  | Not_implemented -> not_implemented test_id s

let test_arit4 test_id s f ppa1 a1 ppa2 a2 ppa3 a3 ppa4 a4 ppb b =
  try
    let y = f a1 a2 a3 a4 in
    if y = b then print_ok test_id
    else
      let message () =
        Format.printf
          "@[function %s@]@\n@[<2>arg1: %a@]@\n@[<2>arg2: %a@]@\n@[<2>arg3: %a@]@\n@[<2>arg4: %a@]@\n@[<2>res: %a@]@\n@[<2>should be %a@]"
          s ppa1 a1 ppa2 a2 ppa3 a3 ppa4 a4 ppb y ppb b
      in
      print_ko test_id message
  with
  | Not_implemented -> not_implemented test_id s

let test_arit5 test_id s f ppa1 a1 ppa2 a2 ppa3 a3 ppa4 a4 ppa5 a5 ppb b =
  try
    let y = f a1 a2 a3 a4 a5 in
    if y = b then print_ok test_id
    else
      let message () =
        Format.printf
          "@[function %s@]@\n@[<2>arg1: %a@]@\n@[<2>arg2: %a@]@\n@[<2>arg3: %a@]@\n@[<2>arg4: %a@]@\n@[<2>arg5: %a@]@\n@[<2>res: %a@]@\n@[<2>should be %a@]"
          s ppa1 a1 ppa2 a2 ppa3 a3 ppa4 a4 ppa5 a5 ppb y ppb b
      in
      print_ko test_id message
  with
  | Not_implemented -> not_implemented test_id s

let finish () =
  if !exists_error then exit 2;
  exit 0
