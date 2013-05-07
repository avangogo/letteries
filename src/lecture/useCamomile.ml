(*gestion de l'encodage*)
open CamomileLibrary
exception ConversionFailed of string
let camomileDir = "/usr/share/camomile";;

module CamomileDefault =
struct
  let datadir    = camomileDir^"/database";;
  let localedir  = camomileDir^"/locales";;
  let charmapdir = camomileDir^"/charmaps";;
  let unimapdir  = camomileDir^"/mappings";;
end

module Interface = CamomileLibrary.CharEncoding.Configure (CamomileDefault);;
module InterfaceType = Interface.Make (CamomileLibrary.UTF8);;

let fmt = Interface.of_name "ISO_8859-1";;

let find_error convert s =
  let rec reduce_until_work reduce s0 =
    let s1 = reduce s0 in
    (try
      convert s1; s0
     with
       |_ -> reduce_until_work reduce s1) in
 (* let red1 s = String.sub s 1 ((String.length s) - 1) in*)
  let red2 s = String.sub s 0 ((String.length s) - 1) in
  reduce_until_work red2 s;;

let latin0_of_utf8 s =
  try
    InterfaceType.encode fmt s
  with
    |_ -> raise (ConversionFailed s);;

let utf8_of_latin0 s =
  try
    InterfaceType.decode fmt s
  with
    |_ -> raise (ConversionFailed s);;
