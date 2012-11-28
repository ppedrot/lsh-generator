(* #!/usr/bin/ocaml *)

open Printf

module Html = Dom_html

type gender = M | F
(* J'ai mis M avant F, je suis un dangereux phallocrate ! *)

type article = Def | Udf | Nop

let nouns = [
  "analyse", F;
  "aporie", F;
  "critique", F;
  "entéléchie", F;
  "monade", F;
  "psychologie", F;
  "hubris", F;
  "hypothèse", F;
  "histoire", F;
  "historiographie", F;
  "littérature", F;
  "ontogénie", F;
  "ontologie", F;
  "pensée", F;
  "phénoménologie", F;
  "philosophie", F;
  "poésie", F;
  "nécessité", F;
  "sémiotique", F;
  "science", F;

  "absolu", M;
  "absurde", M;
  "absolutisme", M;
  "argument", M;
  "féminisme", M;
  "immédiat", M;
  "impératif", M;
  "logicisme", M;
  "modernisme", M;
  "mythe", M;
  "objet", M;
  "objectivisme", M;
  "positivisme", M;
  "réductivisme", M;
  "relatif", M;
  "relativisme", M;
  "signifiant", M;
  "signifié", M;
  "subjectivisme", M;
  "sujet", M;
  "tropisme", M;
]

let prefix = [
  "proto";
  "pré";
  "méta";
  "hyper";
]

let adjectives = [
  "aristotélicien";
  "absolu";
  "culturel";
  "catégorique";
  "cartésien";
  "comparé";
  "critique";
  "freudien";
  "jubilatoire";
  "jouissif";
  "kantien";
  "littéraire";
  "marxiste";
  "monadique";
  "objectif";
  "ontologique";
  "performatif";
  "phénoménologique";
  "poétique";
  "politique";
  "post-moderne";
  "primitif";
  "platonicien";
  "relatif";
  "scientifique";
  "sensible";
  "sémantique";
  "sémiotique";
  "subjectif";
  "théâtral";
  "transcendental";
]

let actions = [
  "réflexion";
  "discussion";
  "étude";
  "analyse";
  "critique";
  "comparaison";
]

let verbs = [
  "analyser";
  "conclure";
  "construire";
  "créer";
  "déconstruire";
  "décrire";
  "déduire";
  "dépasser";
  "ériger";
  "explorer";
  "exposer";
  "fonder";
  "franchir";
  "relativiser";
  "transcender";
  "synthétiser";
]

let names = [
  "Socrate";
  "Platon";
  "Aristote";
  "Leibniz";
  "Freud";
  "Kant";
  "Marx";
  "Descartes";
  "Hegel";
  "Bergson";
]

(*

TODO :

Verbe X
Le X de Y 
X et Y, Z ?
de X à Y : Z
X à travers Y : A et B
X dans l'oeuvre de Y : de A à B
... : A dans B

*)


let () = Random.self_init ()

let starts_with s prefix =
  let pl = String.length prefix in
  if pl > String.length s then false
  else
    let i = ref 0 in
    let break = ref true in
    let () = while !i < pl && !break do
      if s.[!i] = prefix.[!i] then incr i
      else break := false
    done in
    !break

let ends_with s suffix =
  let pl = String.length suffix in
  let len = String.length s in
  if pl > len then false
  else
    let i = ref 0 in
    let break = ref true in
    let () = while !i < pl && !break do
      if s.[len - !i - 1] = suffix.[pl - !i - 1] then incr i
      else break := false
    done in
    !break

let random l =
  let len = List.length l in
  let n = Random.int len in
  List.nth l n

let put_udf_art name = function
| M -> "un " ^ name
| F -> "une " ^ name

let put_def_art name gender =
  let vowels = ["a"; "e"; "i"; "o"; "u"; "y"; "é"; "ê"; "ô"; "â"] in
  let h = List.map (fun s -> "h" ^ s) vowels in
  if List.exists (starts_with name) vowels || List.exists (starts_with name) h then
    "l’" ^ name
  else
    match gender with
    | M -> "le " ^ name
    | F -> "la " ^ name

let decline_adj adj = function
| M -> adj
| F ->
  if ends_with adj "en" then
    adj ^ "ne"
  else if ends_with adj "f" then
    (String.sub adj 0 (String.length adj - 2)) ^ "ive"
  else if ends_with adj "el" then
    adj ^ "le"
  else if ends_with adj "e" then
    adj
  else
    adj ^ "e"

let put_art s gender = function
| Def -> put_def_art s gender
| Udf -> put_udf_art s gender
| Nop -> s

let complex_noun style () =
  let (noun, g) = random nouns in
  let complement = Random.int 3 in
  if complement = 0 then put_art noun g style
  else if complement = 1 then
    let (cmpl, gc) = random nouns in
    let cmpl = put_def_art cmpl gc in
    if starts_with cmpl "le " then
      sprintf "%s du %s" (put_art noun g style) (String.sub cmpl 3 (String.length cmpl - 3))
    else
      sprintf "%s de %s" (put_art noun g style) cmpl
  else
    let adj = decline_adj (random adjectives) g in
    sprintf "%s %s" (put_art noun g style) adj

let type_1 () =
  let wl = complex_noun Nop () in
  let wr = complex_noun Nop () in
  let cc = complex_noun Udf () in
  let qm = if Random.bool () then "?" else "" in
  sprintf "%s et %s : %s %s" wl wr cc qm

let type_2 () =
  let author = random names in
  let cmpl = complex_noun Def () in
  let cc = complex_noun Udf () in
  let qm = if Random.bool () then "?" else "" in
  sprintf "%s et %s : %s %s" author cmpl cc qm

let type_3 () =
  let wl = complex_noun Def () in
  let verb = random verbs in
  let cc = complex_noun Def () in
  if Random.bool () then
    sprintf "%s : %s %s" wl verb cc
  else
    sprintf "%s %s" verb cc

let type_4 () =
  let wl = complex_noun Def () in
  let wr = complex_noun Def () in
  sprintf "%s dans %s" wl wr

let types = [type_1; type_2; type_3; type_4]

let generate () =
  String.capitalize (random types ())

(** Payload for the javascript document *)

let onload _  =
  let doc = Html.document in
(*   let body = doc##body in *)
  let body = doc##getElementById (Js.string "lshfield") in
  let body = Js.Opt.case body (fun () -> assert false) (fun x -> x) in
  let div = Html.createDiv doc in
  let display = Html.createP doc in
  let button = Html.createButton doc in
  let () = button##innerHTML <- (Js.string "Générer un titre") in
  let () = Dom.appendChild body button in
  let () = Dom.appendChild body div in
  let () = Dom.appendChild div display in
  let () = div##style##textAlign <- (Js.string "center") in
  let () = div##style##fontSize <- (Js.string "200%") in
  let () = div##style##fontFamily <- (Js.string "palatino, serif") in
  let () = div##style##fontStyle <- (Js.string "italic") in
  let onclick _ =
    let title = generate () in
    let () = display##innerHTML <- (Js.string title) in
    Js._true
  in
  let () = button##onclick <- Html.handler onclick in
  Js._false

let () = Html.window##onload <- Html.handler onload
