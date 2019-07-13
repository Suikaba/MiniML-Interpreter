open OUnit2
open Miniml

let union_find_test _ =
  let merge_f = (fun x _ -> x) in
  let t1 = Unionfind.create 0 in
  let t2 = Unionfind.create 1 in
  let t3 = Unionfind.create 2 in
  Unionfind.union t1 t2 merge_f;
  assert_equal (Unionfind.is_same t1 t2) true;
  assert_equal (Unionfind.is_same t1 t3) false;
  assert_equal (Unionfind.is_same t2 t3) false;
  assert_equal (Unionfind.root t1).rank 1;
  assert_equal (Unionfind.root t2).rank 1;
  assert_equal (Unionfind.root t3).rank 0;

  let t4 = t2 in
  Unionfind.union t4 t3 merge_f;
  assert_equal (Unionfind.is_same t1 t3) true;

  let t1 = Unionfind.create 0 in
  let t2 = Unionfind.create 1 in
  let t3 = Unionfind.create 2 in
  let t4 = Unionfind.create 3 in
  Unionfind.union t1 t2 merge_f;
  Unionfind.union t3 t4 merge_f;
  Unionfind.union t2 t4 merge_f;
  assert_equal (Unionfind.root t1) (Unionfind.root t2);
  assert_equal (Unionfind.root t1) (Unionfind.root t3);
  assert_equal (Unionfind.root t1) (Unionfind.root t4);
  assert_equal (Unionfind.root t2) (Unionfind.root t3);
  assert_equal (Unionfind.root t2) (Unionfind.root t4);
  assert_equal (Unionfind.root t3) (Unionfind.root t4);
  assert_equal (Unionfind.get_value t1) 0;
  assert_equal (Unionfind.get_value t2) 0;
  assert_equal (Unionfind.get_value t3) 0;
  assert_equal (Unionfind.get_value t4) 0

let suite =
  "suite">:::["test1">:: union_find_test]

let () = run_test_tt_main suite
