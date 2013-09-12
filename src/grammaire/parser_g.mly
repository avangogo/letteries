%{

 exception Grammar_parser of string;;
 
 open Finiteautomaton
 let sigma = Tag.nbre;;
 let assoc_list = ref [];;
 let add k i = assoc_list := (k, i) :: !assoc_list;;
 let assoc id =
   try
     List.assoc id !assoc_list
   with
   | Not_found -> raise (Grammar_parser ("Unknwon id : "^id))    

%}

/* description des lexèmes */

%token EOF
%token PIPE
%token <string> IDPOINTS
%token STAR
%token <Tag.tag> TAG
%token <string> ID

%start main
%type <Finiteautomaton.nondeterministic> main
/* on _doit_ donner le type du point d'entrée */

%%
main:
  | ID def EOF         { let _ = $2 in assoc $1 }


def:
 | IDPOINTS PIPE sum def { add $1 ($3 ()) }
 | { () }

sum:
 | concat { $1 }
 | concat PIPE sum { fun () -> nd_union ($1 ()) ($3 ()) }

concat:
  | elems concat { fun () -> nd_concat ($1 ()) ($2 ()) }
  | { fun () ->  nd_epsilon sigma }

elems:
  | elem STAR { fun () -> nd_star ($1 ()) }
  | elem { $1 }

elem:
  | TAG { fun () -> nd_letter sigma (Tag.int_of_tag $1) }
  | ID  { fun () -> assoc $1 }
