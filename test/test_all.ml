let () =
  Testlib.wrap "Assoc" Test_assoc.main ();
  Testlib.wrap "Matrix" Test_matrix.main ();
  Testlib.finish ()
