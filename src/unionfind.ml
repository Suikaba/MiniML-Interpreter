
type 'a root = { mutable rank: int; }

type 'a t = { mutable parent : 'a node; }
and 'a node =
    Root of 'a root (* it is root (that is, representitive) *)
  | Parent of 'a t  (* it has parent node *)

let create () = { parent = Root { rank = 0; }}

(* compress path to root *)
let compress t =
  let rec compress_impl t path = match t.parent with
      Root _ ->
        let par = Parent t in
        List.iter (fun v -> v.parent <- par) path;
    | Parent t' -> compress_impl t' (t :: path)
  in compress_impl t []

(* get root 'node' (, and at the same time, compress path from t to root *)
(* @return : root node *)
let representitive t =
  compress t;
  match t.parent with
      Root _ -> t
    | Parent p ->
        match p.parent with
          Root _ -> p
        | Parent _ -> failwith "Unionfind: Failed to compress"

(* get root *)
let root t = match (representitive t).parent with
  | Root r -> r
  | _ -> failwith "Unionfind structure is broken"

let is_same t1 t2 = (root t1) == (root t2)

let union t1 t2 =
  let r1 = representitive t1 in
  let r2 = representitive t2 in
  if r1 == r2 then ()
  else
    let rt1 = root r1 in
    let rt2 = root r2 in
    if rt1.rank < rt2.rank then
      r1.parent <- Parent r2
    else begin
      r2.parent <- Parent r1;
      if rt1.rank = rt2.rank then rt1.rank <- rt1.rank + 1;
    end
