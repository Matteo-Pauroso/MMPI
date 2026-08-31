open Alcotest
open Logica

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
      ("implicazione", [ test_case "implica" `Quick test_implica ]);
      ("se e solo se", [ test_case "se_e_solo_se" `Quick test_se_e_solo_se ]);
      ("xor", [ test_case "xor" `Quick test_xor ]);
    ]
