
module IntArrayMap = Map.Make( 
  struct
    type t = int array
    let compare = compare
  end
);;

exception Found of int

let przelewanka (cups : (int * int) array) : int =

  let n = Array.length cups
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

  let (heights, after) = array_split cups
  in

  let start n = Array.make n 0
  in

  let d = nwd_array heights
  in

  if d = 0 then 0 else

  let can_be_done =
    Array.for_all (fun h -> h mod d = 0) after
    &&
    Array.exists (fun (h, o) -> h = o || o = 0) cups
  in

  let q = Queue.create ()
  in

  let vis = ref IntArrayMap.empty
  in

  let until_full x state = 
    let state = Array.copy state in
    state.(x) <- heights.(x);
    state
  in

  let until_empty x state = 
    let state = Array.copy state in
    state.(x) <- 0;
    state
  in

  (* Przelej x do y *)
  let pour state_def y x =
    let state = Array.copy state_def in 
    let left = heights.(y) - state.(y) in 
    if left >= state.(x) then begin
      state.(y) <- state.(y) + state.(x);
      state.(x) <- 0;
    end else begin
      state.(y) <- heights.(y);
      state.(x) <- state.(x) - left
    end;
    state
  in

  Queue.add (start n) q;

  vis := IntArrayMap.add (start n) 0 !vis;

  try 
    while Queue.is_empty q |> not && can_be_done do
      let front = Queue.take q in
      let dis = IntArrayMap.find front !vis in
      if front = after then raise (Found dis);
      let process new_way = 
        if IntArrayMap.mem new_way !vis |> not then begin
          vis := IntArrayMap.add new_way (dis + 1) !vis;
          Queue.add new_way q
        end
      in
      Array.iteri (fun ele va ->
          if va <> 0 then process (until_empty ele front);
          if va <> heights.(ele) then process (until_full ele front); 
          Array.iteri (fun ele2 _ -> 
              if ele <> ele2 && heights.(ele2) <> 0 && heights.(ele) <> va then begin
                process (pour front ele ele2)
              end 
            ) front;
        ) front
    done;
    raise (Found (-1))
  with
  | Found x -> x


