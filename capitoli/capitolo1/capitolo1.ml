(** Insiemi

    Con il termine insieme intendiamo qualsiasi raggruppamento in un tutto unico
    di oggetti determinati e ben distinti della nostra intuizione o del nostro
    pensiero (i quali vengono chiamati gli elementi dell'insieme stesso). ~
    Georg Cantor (1895)

    Per essere definito un insieme, deve essere sempre possibile stabilire
    l'appartenenza di un dato elemento, escludendo quindi insiemi soggettivi. *)

(** Per convenzione gli insiemi si indicano con una lettera maiuscola e i loro
    elementi con la minuscola. *)

let elemento_a = 1
let elemento_b = 2
let elemento_c = 3

(** Questo non è un vero insieme perché in OCaml le liste possono possedere
    duplicati *)
let insieme_A = [ elemento_a; elemento_b; elemento_c ]

(** Si dice che un elemento appartiene a un insieme:

    a∈ A

    Quando un elemento non appartiene ad un insieme:

    a∉ A*)

let appartiene elemento insieme = List.mem elemento insieme

let _ =
  Printf.printf "==========================================\n";
  Printf.printf "a appartiene ad A\n";
  Printf.printf "%b\n" (appartiene elemento_a insieme_A)

(** Sottoinsiemi

    Un insieme è il sottoinsieme di un altro insieme quando tutti i suoi
    elementi sono presenti in tale insieme.

    Un insieme B sottoinsieme di A si indica come:

    B⊆ A*)

let insieme_B = [ elemento_a; elemento_c ]
let insieme_vuoto = []

let sottoinsieme candidato insieme =
  List.for_all (fun e -> List.mem e insieme) candidato

let _ =
  Printf.printf "==========================================\n";
  Printf.printf "B sottoinsieme di A\n";
  Printf.printf "%b\n\n" (sottoinsieme insieme_B insieme_A);
  Printf.printf "insieme vuoto sottoinsieme di A\n";
  Printf.printf "%b\n" (sottoinsieme insieme_vuoto insieme_A)

(** Operazioni sugli insiemi:

    --------------------------------------------------------------------------

    Unione (∪ ):

    Il risultato dell'unione sono tutti gli elementi dei due insiemi in un unico
    gruppo.

    --------------------------------------------------------------------------

    Intersezione (∩ ):

    Il risultato dell'intersezione sono gli elementi presenti in entrambi gli
    insiemi.

    --------------------------------------------------------------------------

    Differenza (-):

    Il risultato della differenza è ciò che resta dell'insieme a sinistra
    rimuovendo gli elementi dell'insieme a destra.

    --------------------------------------------------------------------------

    Complemento ( I ̅ o Iᶜ):

    Tutti gli elementi al di fuori dell'insieme. *)

let unione insieme_1 insieme_2 =
  List.fold_left
    (fun acc e -> if List.mem e acc then acc else acc @ [ e ])
    insieme_1 insieme_2

let intersezione insieme_1 insieme_2 =
  List.filter (fun e -> List.mem e insieme_2) insieme_1

let differenza insieme_1 insieme_2 =
  List.filter (fun e -> not (List.mem e insieme_2)) insieme_1

(** Il complemento è una differenza U (universo) - insieme. *)
let complemento universo insieme =
  List.filter (fun e -> not (List.mem e insieme)) universo

let _ =
  Printf.printf "==========================================\n";
  Printf.printf "A unione B: ";
  List.iter (Printf.printf "%d ") (unione insieme_A insieme_B);
  Printf.printf "\n";

  Printf.printf "A intersezione B: ";
  List.iter (Printf.printf "%d ") (intersezione insieme_A insieme_B);
  Printf.printf "\n";

  Printf.printf "A differenza B: ";
  List.iter (Printf.printf "%d ") (differenza insieme_A insieme_B);
  Printf.printf "\n";

  Printf.printf "Complemento di B usando A come universo: ";
  List.iter (Printf.printf "%d ") (complemento insieme_A insieme_B);
  Printf.printf "\n"

(** https://ocaml.org/docs/sets#sets

    A quanto pare OCaml mette a disposizione di suo gli insiemi. *)

module IntSet = Set.Make (Int)

let insieme_C =
  [ elemento_a; elemento_a; elemento_b; elemento_c ] |> IntSet.of_list

let _ =
  Printf.printf "==========================================\n";
  Printf.printf "Set invece che liste: ";
  List.iter (Printf.printf "%d ") (IntSet.to_list insieme_C);
  Printf.printf "\n"

(** Inoltre Set espone anche metodi per "unione", "intersezione", etc.

    L'unico non esposto è il complemenentare visto che necessità di un universo.
*)

(** Si può facilmente dimostrare che:

    - I unione I = I;
    - I unione O = I;
    - I intersezione I = I;
    - I intersezione O = O;

    Dove O è l'insieme vuoto. *)

let i_I = insieme_C

let _ =
  Printf.printf "==========================================\n";
  Printf.printf "I = ";
  List.iter (Printf.printf "%d ") (IntSet.to_list i_I);
  Printf.printf "\n\n";
  Printf.printf "I unione I = I\n";
  List.iter (Printf.printf "%d ") (IntSet.to_list (IntSet.union i_I i_I));
  Printf.printf "\n";
  Printf.printf "\n";
  Printf.printf "I unione O = I\n";
  List.iter (Printf.printf "%d ")
    (IntSet.to_list (IntSet.union i_I IntSet.empty));
  Printf.printf "\n";
  Printf.printf "\n";
  Printf.printf "I intersezione I = I\n";
  List.iter (Printf.printf "%d ") (IntSet.to_list (IntSet.inter i_I i_I));
  Printf.printf "\n";
  Printf.printf "\n";
  Printf.printf "I intersezione O = O\n";
  List.iter (Printf.printf "%d ")
    (IntSet.to_list (IntSet.inter i_I IntSet.empty))

(** L' ultima verifica non stampa nulla visto che il nuovo insieme è vuoto.*)
