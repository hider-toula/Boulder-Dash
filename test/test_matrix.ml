open Testlib
open Structures
open Matrix

let main () =
  let pp_string = (fun fmt s -> Format.fprintf fmt "\"%s\"" s) in
  let pp_matrix_string = pp pp_string in
  let tester0 =
    { larg = 4;
      haut = 3;
      map =
        Assoc.(
          {
            assoc =
              [
                (0, {assoc = [(0,"aa"); (3, "dd")]; default = ""});
                (2, {assoc = [(1,"bb"); (2, "cc")]; default = "uu"})
              ];
            default =
              Assoc.(
                {
                  assoc = [(1, "b"); (2, "c")];
                  default = ""
                }
              )
          }
        )
    }
  in
  let tester1 =
    { tester0 with
      map =
        Assoc.(
          { tester0.map with
            assoc =
              [
                (0, {assoc = [(0,"aa"); (1, "bb"); (3, "dd")]; default = ""});
                (2, {assoc = [(1,"bb"); (2, "cc")]; default = "uu"})
              ];
          }
        )
    }
  in
  let tester2 =
    { tester0 with
      map =
        Assoc.(
          { tester0.map with
            assoc =
              [
                (0, {assoc = [(0,"aa"); (3, "dd")]; default = ""});
                (1, {assoc = [(1, "aa"); (2, "c")]; default = ""});
                (2, {assoc = [(1,"bb"); (2, "cc")]; default = "uu"})
              ];
          }
        )
    }
  in
    let tester3 =
    { tester0 with
      map =
        Assoc.(
          { tester0.map with
            assoc =
              [
                (0, {assoc = [(0,"aa"); (3, "uu")]; default = ""});
                (2, {assoc = [(1,"bb"); (2, "cc")]; default = "uu"})
              ];
          }
        )
    }
  in
  (* MAKE *)
  let () = test_arit3 0 "make"
      make
      Format.pp_print_int 4
      Format.pp_print_int 3
      pp_string ""
      pp_matrix_string {larg = 3; haut = 4; map = {assoc = []; default = {assoc = []; default = ""}}}
  in

  (* READ *)
  let () = test_arit3 1 "read"
      read
      Format.pp_print_int 2
      Format.pp_print_int 3
      pp_matrix_string tester0
      pp_string "uu"
  in
  let () = test_arit3 2 "read"
      read
      Format.pp_print_int 1
      Format.pp_print_int 1
      pp_matrix_string tester0
      pp_string "b"
  in
  let () = test_arit3 3 "read"
      read
      Format.pp_print_int 0
      Format.pp_print_int 0
      pp_matrix_string tester0
      pp_string "aa"
  in
  let () = test_arit3 4 "read"
      read
      Format.pp_print_int 0
      Format.pp_print_int 1
      pp_matrix_string tester0
      pp_string ""
  in

  (* SET *)
  let () = test_arit4 5 "set"
      set
      Format.pp_print_int 0
      Format.pp_print_int 1
      pp_string "bb"
      pp_matrix_string tester0
      pp_matrix_string tester1
  in
  let () = test_arit4 6 "set"
      set
      Format.pp_print_int 1
      Format.pp_print_int 1
      pp_string "aa"
      pp_matrix_string tester0
      pp_matrix_string tester2
  in
  let () = test_arit4 7 "set"
      set
      Format.pp_print_int 0
      Format.pp_print_int 3
      pp_string "uu"
      pp_matrix_string tester0
      pp_matrix_string tester3
  in

  (* FOLD *)
  let print_func = (fun fmt _ -> Format.fprintf fmt "(fun i j s acc -> acc^(string_of_int i)^(string_of_int j)^s)") in
  let () = test_arit3 8 "fold"
      fold
      print_func (fun i j s acc -> acc^(string_of_int i)^(string_of_int j)^s)
      pp_matrix_string tester0
      pp_string ""
      pp_string "00aa010203dd1011b12c1320uu21bb22cc23uu"
  in
  ()
