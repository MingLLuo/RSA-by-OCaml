open OUnit2
open Rsa

let test_coprime _ =
  assert_equal true (coprime (Z.of_int 5) (Z.of_int 7));
  assert_equal false (coprime (Z.of_int 4) (Z.of_int 6));
  assert_equal true (coprime (Z.of_int 3) (Z.of_int 8))

let test_prime_phi _ =
  assert_equal (Z.of_int 1) (prime_phi []);
  assert_equal (Z.of_int 2) (prime_phi [ Z.of_int 2; Z.of_int 3 ]);
  assert_equal (Z.of_int 12) (prime_phi [ Z.of_int 7; Z.of_int 3 ])

let test_mod_exp _ =
  assert_equal (Z.of_int 1) (mod_exp (Z.of_int 2) (Z.of_int 0) (Z.of_int 3));
  assert_equal (Z.of_int 3) (mod_exp (Z.of_int 2) (Z.of_int 3) (Z.of_int 5));
  assert_equal (Z.of_int 1) (mod_exp (Z.of_int 2) (Z.of_int 10) (Z.of_int 11))

let test_mod_minv _ =
  assert_equal (Z.of_int 2) (mod_minv (Z.of_int 3) (Z.of_int 5));
  assert_equal (Z.of_int 3) (mod_minv (Z.of_int 7) (Z.of_int 10));
  assert_equal (Z.of_int 1) (mod_minv (Z.of_int 1) (Z.of_int 7))

let suite =
  "suite"
  >::: [
         "test_coprime" >:: test_coprime;
         "test_prime_phi" >:: test_prime_phi;
         "test_mod_exp" >:: test_mod_exp;
         "test_mod_minv" >:: test_mod_minv;
       ]

let () = run_test_tt_main suite
