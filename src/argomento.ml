type formula = Tavola_verita.valutazione -> bool

let valido numero_variabili premesse conclusione =
  List.for_all
    (fun valutazione ->
      let premesse_verificate =
        List.for_all (fun premessa -> premessa valutazione) premesse
      in

      (not premesse_verificate) || conclusione valutazione)
    (Tavola_verita.valutazioni numero_variabili)

let controesempio numero_variabili premesse conclusione =
  List.find_opt
    (fun valutazione ->
      let premesse_verificate =
        List.for_all (fun premessa -> premessa valutazione) premesse
      in

      premesse_verificate && not (conclusione valutazione))
    (Tavola_verita.valutazioni numero_variabili)
