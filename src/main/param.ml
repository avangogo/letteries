(* 
   Lettreries is a random poem generator.
    Copyright (C) 2012 Rémi de Verclos

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

type task =
  |PoemFromComputed
  |PoemFromCorpus
  |MakeComputed;;

type loquacity =
  | Quiet
  | Usual
  | Verbose

(* action to perform *)
let task = ref PoemFromComputed

(* seeds *)
let first_word = ref "bleu"
let seed = ref (Random.self_init (); Random.int (1 lsl 29))

(* output *)
let output = ref None

(* information writing *)
let loquacity = ref Usual

(* locations and directories *)
let corpus_dir     = ref "data/corpus/"
let corpus_subdirs = ref
  [ "Les Châtiments/";
    "Poésies(Mallarmé,1914)/";
    "Les fleurs du mal (1868)/";
    "Rimbaud, Poésies/";
    "usr/" ]
let computed_dir   = ref "data/computed/"
let tmp_dir        = ref "tmp/"
let phoneticrules_file = ref "data/reglesphonetiques"
let treetagger_script = ref "./treetagger/cmd/tree-tagger-french"

(* algorithm parameters *)
let minSentenceLength = ref 10
let maxSentenceLength = ref 30
let maxTries = ref 50 (* Nombre maximal de backtracking sur un mot *)

(* poem form *)
let poemLength = ref 6

(* reading arguments *)
let set_output s = output := Some s

let descr = "Lettreries is a random poem generator."


let output_spec =
  (
    "-o",
    Arg.String set_output,
    "<file> The output file."
  )
    
let quietmod_spec =
  (
    "-q",
    Arg.Unit (fun () -> loquacity := Quiet),
    " Write nothing but the poem"
  )
    
let verbosemod_spec =
  (
    "-v",
    Arg.Unit (fun () -> loquacity := Verbose),
    " Say more"
  )

let seed_spec =
  (
    "-s",
    Arg.Set_int seed,
    "<int> The seed for the random generator."
  )
    
let first_spec =
  (
    "-f",
    Arg.Set_string first_word,
    "<string> The seed word for the algorithm."
  )

    
let poemlength_spec =
  (
    "-l",
    Arg.Set_int poemLength,
    "<int> The number of verses to be printed."
  )

let without_treetagger_spec =
  (
    "-old",
    Arg.Unit (fun () -> task := PoemFromCorpus),
    " Run the algorithm directly on the corpus. Do not use Treetagger."
  )

let makecorpus_spec =
  (
    "-makecorpus",
    Arg.Unit (fun () -> task := MakeComputed),
    " Build the tagged texts with Treetagger. Do nothing else."
  )
 
let spec = Arg.align [output_spec; seed_spec; first_spec; poemlength_spec; quietmod_spec; verbosemod_spec; without_treetagger_spec; makecorpus_spec]
 
let empty_anon_fun s =
  raise (Arg.Bad ( Printf.sprintf "Don't know what to do with %s" s ))

let parse_arg () = Arg.parse spec empty_anon_fun descr
  
