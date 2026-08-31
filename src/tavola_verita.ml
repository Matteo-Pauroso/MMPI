type valutazione = bool array

let valutazioni n =
  let rec genera dimensione =
    if dimensione = 0 then [ [||] ]
    else
      let precedenti = genera (dimensione - 1) in

      List.concat_map
        (fun valutazione ->
          [
            Array.append valutazione [| false |];
            Array.append valutazione [| true |];
          ])
        precedenti
  in

  genera n

let valuta formula valutazione = formula valutazione

let tautologia n formula =
  List.for_all (fun valutazione -> valuta formula valutazione) (valutazioni n)

let contraddizione n formula =
  List.for_all
    (fun valutazione -> not (valuta formula valutazione))
    (valutazioni n)

let soddisfacibile n formula =
  List.exists (fun valutazione -> valuta formula valutazione) (valutazioni n)

let equivalenti n formula1 formula2 =
  List.for_all
    (fun valutazione ->
      valuta formula1 valutazione = valuta formula2 valutazione)
    (valutazioni n)
