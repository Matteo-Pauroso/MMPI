open Alcotest
open Logica
module IntSet = Set.Make (Int)
module StringSet = Set.Make (String)
module IntInt = Prodotto_cartesiano (IntSet) (IntSet)
module IntString = Prodotto_cartesiano (IntSet) (StringSet)

let test_prodotto_cartesiano_int () =
  let insieme_1 = IntSet.of_list [ 1; 2 ] in
  let insieme_2 = IntSet.of_list [ 3; 4 ] in

  let risultato = IntInt.calcola insieme_1 insieme_2 |> List.sort compare in

  let atteso = [ (1, 3); (1, 4); (2, 3); (2, 4) ] |> List.sort compare in

  check (list (pair int int)) "prodotto cartesiano di interi" atteso risultato

let test_prodotto_cartesiano_misti () =
  let insieme_1 = IntSet.of_list [ 1; 2 ] in
  let insieme_2 = StringSet.of_list [ "a"; "b" ] in

  let risultato = IntString.calcola insieme_1 insieme_2 |> List.sort compare in

  let atteso =
    [ (1, "a"); (1, "b"); (2, "a"); (2, "b") ] |> List.sort compare
  in

  check
    (list (pair int string))
    "prodotto cartesiano int-string" atteso risultato

let test_prodotto_cartesiano_vuoto () =
  let insieme_1 = IntSet.empty in
  let insieme_2 = IntSet.of_list [ 1; 2 ] in

  let risultato = IntInt.calcola insieme_1 insieme_2 in

  check
    (list (pair int int))
    "prodotto cartesiano con insieme vuoto" [] risultato

let test_implica () =
  check bool "V implica V" true (implica true true);

  check bool "V implica F" false (implica true false);

  check bool "F implica V" true (implica false true);

  check bool "F implica F" true (implica false false)

let test_se_e_solo_se () =
  check bool "V se e solo se V" true (se_e_solo_se true true);

  check bool "V se e solo se F" false (se_e_solo_se true false);

  check bool "F se e solo se V" false (se_e_solo_se false true);

  check bool "F se e solo se F" true (se_e_solo_se false false)

let test_xor () =
  check bool "V xor V" false (xor true true);

  check bool "V xor F" true (xor true false);

  check bool "F xor V" true (xor false true);

  check bool "F xor F" false (xor false false)

let () =
  run "Logica"
    [
      ( "prodotto cartesiano",
        [
          test_case "interi" `Quick test_prodotto_cartesiano_int;
          test_case "tipi diversi" `Quick test_prodotto_cartesiano_misti;
          test_case "insieme vuoto" `Quick test_prodotto_cartesiano_vuoto;
        ] );
      ("implicazione", [ test_case "implica" `Quick test_implica ]);
      ("se e solo se", [ test_case "se_e_solo_se" `Quick test_se_e_solo_se ]);
      ("xor", [ test_case "xor" `Quick test_xor ]);
    ]
