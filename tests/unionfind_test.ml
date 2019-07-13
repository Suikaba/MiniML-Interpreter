open OUnit2
open Miniml

let union_find_test _ =
  let t1 = Unionfind.create () in
  let t2 = Unionfind.create () in
  let t3 = Unionfind.create () in
  Unionfind.union t1 t2;
  assert_equal (Unionfind.is_same t1 t2) true;
  assert_equal (Unionfind.is_same t1 t3) false;
  assert_equal (Unionfind.is_same t2 t3) false;
  assert_equal (Unionfind.root t1).rank 1;
  assert_equal (Unionfind.root t2).rank 1;
  assert_equal (Unionfind.root t3).rank 0

let suite =
  "suite">:::["test1">:: union_find_test]

let () = run_test_tt_main suite
