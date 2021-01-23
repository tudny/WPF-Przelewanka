module IntArrayMap = Map.Make( 
  struct
    type t = int array
    let compare = compare
  end
);;

exception Found of int

let przelewanka cups = 
  let cups_nr = Array.length cups
  in
  if cups_nr = 0 then 0 else
  
  let array_nwd = 
    let rec nwd x y = 
      if y = 0 then x
      else nwd y (x mod y)
    in Array.fold_left nwd 0
  in 

  let array_split arr =
    let len = Array.length arr in
    let part which = Array.init len (fun i -> which arr.(i)) in 
    (part fst, part snd)
  in

  let heights, result = array_split cups
  in

  let d = array_nwd heights in
  if d = 0 then 0 else
  
  let can_be_done = 
    Array.for_all (fun h -> h mod d = 0) result &&
    Array.exists (fun (h, e) -> h = e || e = 0) cups
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

  let q = Queue.create () in
  let vis = ref IntArrayMap.empty in
  let start_state = Array.make cups_nr 0 in
  Queue.add start_state q;
  vis := IntArrayMap.add (start_state) 0 !vis;

  try 
    while can_be_done && not (Queue.is_empty q) do 
      let front = Queue.take q in 
      let dis = IntArrayMap.find front !vis in 
      if front = result then raise (Found dis);
      
      let process new_way = 
        if not (IntArrayMap.mem new_way !vis) then begin
          vis := IntArrayMap.add new_way (dis + 1) !vis;
          Queue.add new_way q
        end in 
      
      Array.iteri ( fun ele height -> 
          if height <> 0 then process (until_empty ele front);
          if height <> heights.(ele) then process (until_full ele front);

          Array.iteri ( fun ele2 _ ->
              if ele <> ele2 && heights.(ele2) <> 0 &&
               heights.(ele) <> height then begin
                process (pour front ele ele2)
              end
            ) front
        ) front
    done;
    -1
  with
  | Found x -> x

