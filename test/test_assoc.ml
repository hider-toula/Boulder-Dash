open Testlib
open Structures

let main () =
  let pp_string = (fun fmt s -> Format.fprintf fmt "\"%s\"" s) in
  let pp_assoc_string = Assoc.pp_assoc pp_string in
  let pp_map_string = Assoc.pp pp_string in
  let tester0 = [(1, "a"); (3, "c"); (7, "g")] in
  let tester1 = Assoc.({assoc = tester0; default = ""}) in

  (* REMOVE *)
  let () = test_arit2 0 "remove_assoc"
      Assoc.remove_assoc
      Format.pp_print_int 0
      pp_assoc_string tester0
      pp_assoc_string tester0
  in
  let () = test_arit2 1 "remove_assoc"
      Assoc.remove_assoc
      Format.pp_print_int 1
      pp_assoc_string tester0
      pp_assoc_string [(3, "c"); (7, "g")]
  in
  let () = test_arit2 2 "remove_assoc"
      Assoc.remove_assoc
      Format.pp_print_int 7
      pp_assoc_string tester0
      pp_assoc_string [(1, "a"); (3, "c")]
  in
  let () = test_arit2 3 "remove_assoc"
      Assoc.remove_assoc
      Format.pp_print_int 6
      pp_assoc_string tester0
      pp_assoc_string tester0
  in
  let () = test_arit2 4 "remove_assoc"
      Assoc.remove_assoc
      Format.pp_print_int 9
      pp_assoc_string tester0
      pp_assoc_string tester0
  in

  (* CONSTANT  *)
  let () = test_arit1 5 "constant"
      Assoc.constant
      Format.pp_print_string "test"
      pp_map_string (Assoc.({assoc = []; default = "test"}))
  in

  (* FIND *)
  let () = test_arit2 6 "find"
      Assoc.find
      Format.pp_print_int 0
      pp_map_string tester1
      pp_string ""
  in
  let () = test_arit2 7 "find"
      Assoc.find
      Format.pp_print_int 1
      pp_map_string tester1
      pp_string "a"
  in
  let () = test_arit2 8 "find"
      Assoc.find
      Format.pp_print_int 2
      pp_map_string tester1
      pp_string ""
  in
  let () = test_arit2 9 "find"
      Assoc.find
      Format.pp_print_int 3
      pp_map_string tester1
      pp_string "c"
  in
  let () = test_arit2 10 "find"
      Assoc.find
      Format.pp_print_int 7
      pp_map_string tester1
      pp_string "g"
  in
  let () = test_arit2 11 "find"
      Assoc.find
      Format.pp_print_int 44
      pp_map_string tester1
      pp_string ""
  in

  (* SET *)
  let () = test_arit3 12 "set"
      Assoc.set
      Format.pp_print_int 0
      pp_string ""
      pp_map_string tester1
      pp_map_string tester1
  in
  let () = test_arit3 13 "set"
      Assoc.set
      Format.pp_print_int 0
      pp_string "aa"
      pp_map_string tester1
      pp_map_string {tester1 with assoc = [(0, "aa"); (1, "a"); (3, "c"); (7, "g")]}
  in
  let () = test_arit3 14 "set"
      Assoc.set
      Format.pp_print_int 1
      pp_string "aa"
      pp_map_string tester1
      pp_map_string {tester1 with assoc = [(1, "aa"); (3, "c"); (7, "g")]}
  in
  let () = test_arit3 15 "set"
      Assoc.set
      Format.pp_print_int 2
      pp_string "aa"
      pp_map_string tester1
      pp_map_string {tester1 with assoc = [(1, "a"); (2, "aa"); (3, "c"); (7, "g")]}
  in
  let () = test_arit3 16 "set"
      Assoc.set
      Format.pp_print_int 3
      pp_string "aa"
      pp_map_string tester1
      pp_map_string {tester1 with assoc = [(1, "a"); (3, "aa"); (7, "g")]}
  in
  let () = test_arit3 16 "set"
      Assoc.set
      Format.pp_print_int 10
      pp_string "aa"
      pp_map_string tester1
      pp_map_string {tester1 with assoc = [(1, "a"); (3, "c"); (7, "g"); (10, "aa")]}
  in
  let () = test_arit3 17 "set"
      Assoc.set
      Format.pp_print_int 10
      pp_string ""
      pp_map_string tester1
      pp_map_string tester1
  in
  let () = test_arit3 18 "set"
      Assoc.set
      Format.pp_print_int 7
      pp_string ""
      pp_map_string tester1
      pp_map_string {tester1 with assoc = [(1, "a"); (3, "c")]}
  in
  let () = test_arit3 19 "set"
      Assoc.set
      Format.pp_print_int 1
      pp_string ""
      pp_map_string tester1
      pp_map_string {tester1 with assoc = [(3, "c"); (7, "g")]}
  in
  let () = test_arit3 20 "set"
      Assoc.set
      Format.pp_print_int (-1)
      pp_string "aa"
      pp_map_string tester1
      pp_map_string {tester1 with assoc = [(-1,"aa"); (1, "a"); (3, "c"); (7, "g")]}
  in

  (* FOLD *)
  let print_func_conc = fun fmt _ -> Format.fprintf fmt "(fun i x acc -> acc ^ x)" in
  let print_func_conc_i = fun fmt _ -> Format.fprintf fmt "(fun i x acc -> acc ^ (string_of_int i) ^ x)" in
  let () = test_arit5 21 "fold"
      Assoc.fold
      print_func_conc (fun _ x acc -> acc ^ x)
      Format.pp_print_int 0
      Format.pp_print_int 10
      pp_map_string tester1
      pp_string "init"
      pp_string "initacg"
  in

  let () = test_arit5 22 "fold"
      Assoc.fold
      print_func_conc (fun _ x acc -> acc ^ x)
      Format.pp_print_int 0
      Format.pp_print_int 10
      pp_map_string (Assoc.{tester1 with default = "_"})
      pp_string "init"
      pp_string "init_a_c___g___"
  in

  let () = test_arit5 23 "fold"
      Assoc.fold
      print_func_conc_i (fun i x acc -> acc ^ (string_of_int i) ^x)
      Format.pp_print_int 0
      Format.pp_print_int 10
      pp_map_string (Assoc.{tester1 with default = "_"})
      pp_string "init"
      pp_string "init0_1a2_3c4_5_6_7g8_9_10_"
  in

  let () = test_arit5 24 "fold"
      Assoc.fold
      print_func_conc_i (fun i x acc -> acc ^ (string_of_int i) ^x)
      Format.pp_print_int 7
      Format.pp_print_int 10
      pp_map_string (Assoc.{tester1 with default = "_"})
      pp_string "init"
      pp_string "init7g8_9_10_"
  in

  ()
