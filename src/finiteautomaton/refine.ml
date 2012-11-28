
(*nsets elems loc sidx begin end mid*)
type t = int ref * int array * int array * int array * int array * int array * int array
type el = int
type set = int


let make n : t =
  let set = ref 1
  and elems = Array.init n (fun i -> i) 
  and loc = Array.init n (fun i -> i)
  and sidx = Array.make n 0
  and first = Array.make n (-1)
  and end_ = Array.make n (-1)
  and mid =  Array.make n (-1) in
  first.(0) <- 0;
  end_.(0) <- n;
  mid.(0) <- 0;
  set, elems, loc, sidx, first, end_, mid;;

let size ((_, _, _, _, first, end_ ,_ ) :t ) s = end_.(s) - first.(s)

let set ((_, _, _, sidx, _, _, _) :t ) e = sidx.(e)

let first ((_, elems, _, _, first, _, _) :t ) s = elems.(first.(s))

let next ((_, elems, loc, sidx, _, end_, _) :t ) e =
  let i = loc.(e) + 1 in
  if i >= end_.(sidx.(e)) then failwith "next"
  else elems.(i)

let mark ((_, elems, loc, sidx, _, end_, mid) :t ) e =
  let s = sidx.(e)
  and l = loc.(e) in
  let m = mid.(s) in
  if l >= m then
    begin
      elems.(l) <- elems.(m);
      loc.(elems.(l)) <- l;
      elems.(m) <- e;
      loc.(e) <- m;
      mid.(s) <- m + 1
    end

let split ((sets, elems, loc, sidx, first, end_, mid) :t ) s =
  if mid.(s) = end_.(s) then mid.(s) <- first.(s);
  if mid.(s) = first.(s) then (-1)
  else
    begin
      let set = !sets in
      incr sets;
      first.(set) <- first.(s);
      mid.(set)  <- first.(s);
      end_.(set) <- mid.(s);
      first.(s) <- mid.(s);
      for i = first.(set) to end_.(set) - 1 do
	sidx.(elems.(i)) <- set
      done;
      set
    end

let no_marks ((_, _, _, _, first, _, mid) :t ) s =
  mid.(s) = first.(s)



let iter ((_, elems, _, _, first, end_, _) :t ) f s =
  for i = first.(s) to end_.(s) - 1 do
    f elems.(i)
  done;;

let sets ((sets, _, _, _, _, _, _) :t ) =
  !sets;;
