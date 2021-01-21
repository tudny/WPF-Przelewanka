
exception Found of int

let przelewanka (kubki : (int * int) array) : int =

  let n = Array.length kubki
  in

  if n = 0 then 0 else

  let rec nwd x y = 
    if y = 0 then x
    else nwd y (x mod y)
  in

  let nwd_array = Array.fold_left nwd 0
  in

  let array_split arr =
    let len = Array.length arr in
    let part which = Array.init len (fun i -> which arr.(i)) in 
    (part fst, part snd)
  in

  let (heights, after) = array_split kubki
  in

  let start n = Array.make n 0
  in

  let d = nwd_array heights
  in

  if d = 0 then 0 else

  let can_be_done =
    Array.for_all (fun h -> h mod d = 0) after
    &&
    Array.exists (fun (h, o) -> h = o || o = 0) kubki
  in

  let q = Queue.create ()
  in

  let vis = Hashtbl.create 1000
  in

  let do_pelna x stan = 
    let stan = Array.copy stan in
    stan.(x) <- heights.(x);
    stan
  in

  let od_pusta x stan = 
    let stan = Array.copy stan in
    stan.(x) <- 0;
    stan
  in

  (* Przelej x do y *)
  let przelej stan_def y x =
    let stan = Array.copy stan_def in 
    let left = heights.(y) - stan.(y) in 
    if left >= stan.(x) then begin
      stan.(y) <- stan.(y) + stan.(x);
      stan.(x) <- 0;
    end else begin
      stan.(y) <- heights.(y);
      stan.(x) <- stan.(x) - left
    end;
    stan
  in

  Queue.add (start n) q;

  Hashtbl.add vis (start n) 0;

  try 
    while Queue.is_empty q |> not && can_be_done do
      let front = Queue.take q in
      let dis = Hashtbl.find vis front in
      flush stdout;
      if front = after then raise (Found dis);
      let process new_way = 
        if Hashtbl.mem vis new_way |> not then begin
          Hashtbl.add vis new_way (dis + 1);
          Queue.add new_way q
        end
      in
      Array.iteri (fun ele va ->
          if va <> 0 then process (od_pusta ele front);
          if va <> heights.(ele) then process (do_pelna ele front); 
          Array.iteri (fun ele2 _ -> 
              if ele <> ele2 && heights.(ele2) <> 0 && heights.(ele) <> va then begin
                process (przelej front ele ele2)
              end 
            ) front;
        ) front
    done;
    raise (Found (-1))
  with
  | Found x -> x


