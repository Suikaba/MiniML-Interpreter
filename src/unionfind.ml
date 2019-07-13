
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

(* get representitive (, and at the same time, compress path from t to root *)
(* @return : (compressed t, representitive) *)
let representitive t =
  compress t;
  match t.parent with
      Root r -> (t, r)
    | Parent p ->
        match p.parent with
          Root r -> (t, r)
        | Parent _ -> failwith "Unionfind: Failed to compress"

let root t = snd (representitive t)

let is_same t1 t2 = (root t1) == (root t2)

let union t1 t2 =
  let (t1, r1) = representitive t1 in
  let (t2, r2) = representitive t2 in
  if r1 == r2 then ()
  else
    let rank1 = r1.rank in
    let rank2 = r2.rank in
    if rank1 < rank2 then
      t1.parent <- Parent t2
    else begin
      t2.parent <- Parent t1;
      if rank1 = rank2 then r1.rank <- r1.rank + 1;
    end
