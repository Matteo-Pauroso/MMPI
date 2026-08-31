type formula = Tavola_verita.valutazione -> bool

val valido : int -> formula list -> formula -> bool

val controesempio :
  int -> formula list -> formula -> Tavola_verita.valutazione option
