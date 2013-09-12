(* Sauvegarde et chargement du transducteur de phonétique *)
let savePhonetic rulesFile transducerFile =
  Print.p "Précalcul des règles phonétiques ";
  let transducer = Phonetique.make_automate rulesFile in
  let outChannel = open_out transducerFile in
  Marshal.to_channel outChannel (transducer : Phonetique.automate) [];
  close_out outChannel

let makePhonetic rulesFile transducerFile =
  if not (Sys.file_exists transducerFile)
  then savePhonetic rulesFile transducerFile
  else
    let statRules = Unix.stat rulesFile
    and statTransducer = Unix.stat transducerFile in
    if statTransducer.Unix.st_mtime < statRules.Unix.st_mtime
    then savePhonetic rulesFile transducerFile

let loadPhonetic rulesFile transducerFile =
  makePhonetic rulesFile transducerFile;
  Print.p "Chargement des Règles de phonétique ";
  let inChannel = open_in transducerFile in
  let transducer : Phonetique.automate =
    Marshal.from_channel inChannel in
  close_in inChannel;
  transducer
