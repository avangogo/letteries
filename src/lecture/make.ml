let modifDate file =
  try
    let stat = Unix.stat file in
    Some stat.Unix.st_mtime
  with
    Unix.Unix_error (Unix.ENOENT, _, _) -> None

let maxDate x y =
  match x, y with
  | None, _ -> None
  | _, None -> None
  | Some dx, Some dy ->
    Some (if dx > dy then dx else dy)

let toBeUpdate dependList target =
  match modifDate target with
  | None -> true
  | Some targetDate ->
    match
      List.fold_left maxDate (Some min_float) (List.map modifDate dependList)
    with
    | None -> failwith "Misssing file"
    | Some dependDate -> targetDate < dependDate

let applyOption f = function
  | Some x -> f x
  | None -> ()

let make mOption dependList target action =
  if toBeUpdate dependList target
  then
    begin
      applyOption Print.p mOption;
      let outChannel = open_out target in
      Marshal.to_channel outChannel (action ()) [];
      close_out outChannel
    end
      
let load ?makeMessage:mm ?loadMessage:lm dependList target action =
  make mm dependList target action;
  applyOption Print.p lm;
  let inChannel = open_in target in
  let res = Marshal.from_channel inChannel in
  close_in inChannel;
  res
