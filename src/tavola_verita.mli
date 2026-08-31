type valutazione = bool array

val valutazioni : int -> valutazione list
val valuta : (valutazione -> bool) -> valutazione -> bool
val tautologia : int -> (valutazione -> bool) -> bool
val contraddizione : int -> (valutazione -> bool) -> bool
val soddisfacibile : int -> (valutazione -> bool) -> bool
val equivalenti : int -> (valutazione -> bool) -> (valutazione -> bool) -> bool
