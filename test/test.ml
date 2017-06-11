open Cs3110
open OUnit2

let suite =
  "suite" >::: [
    "test note03 sum" >:: (fun _ -> assert_equal 0 (Note03.sum []));
    "test rec02 inc" >:: (fun _ -> assert_equal 2 (Rec02.inc 1))
  ]

let () = run_test_tt_main suite
