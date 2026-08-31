open Alcotest
open Logica
open Tavola_verita

let test_numero_valutazioni () =
  check int "2 variabili producono 4 valutazioni" 4
    (List.length (valutazioni 2));

  check int "3 variabili producono 8 valutazioni" 8
    (List.length (valutazioni 3))

let test_tautologia () =
  let formula v =
    let p = v.(0) in
    let q = v.(1) in

    implica p q || implica q p
  in

  check bool "p implica q oppure q implica p è una tautologia" true
    (tautologia 2 formula)

let test_contraddizione () =
  let formula v = let p = v.(0) in

                  p && not p in

  check bool "p e non p è una contraddizione" true (contraddizione 1 formula)

let test_soddisfacibile () =
  let formula v = v.(0) in

  check bool "p è soddisfacibile" true (soddisfacibile 1 formula)

let test_equivalenza () =
  let formula1 v =
    let p = v.(0) in
    let q = v.(1) in

    implica p q
  in

  let formula2 v =
    let p = v.(0) in
    let q = v.(1) in

    (not p) || q
  in

  check bool "p implica q equivale a non p oppure q" true
    (equivalenti 2 formula1 formula2)

let () =
  run "Tavola di verità"
    [
      ( "valutazioni",
        [ test_case "numero di valutazioni" `Quick test_numero_valutazioni ] );
      ("tautologia", [ test_case "tautologia" `Quick test_tautologia ]);
      ( "contraddizione",
        [ test_case "contraddizione" `Quick test_contraddizione ] );
      ( "soddisfacibilità",
        [ test_case "soddisfacibilità" `Quick test_soddisfacibile ] );
      ("equivalenza", [ test_case "equivalenza" `Quick test_equivalenza ]);
    ]
