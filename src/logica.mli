module type SET = Set.S

module Prodotto_cartesiano : functor (S1 : SET) -> functor (S2 : SET) -> sig
  val calcola : S1.t -> S2.t -> (S1.elt * S2.elt) list
end

val implica : bool -> bool -> bool
(** [implica p q] calcola l'implicazione logica [p -> q].

    L'implicazione è falsa solamente quando [p] è vera e [q] è falsa. *)

val se_e_solo_se : bool -> bool -> bool
(** [se_e_solo_se] calcola l'equivalenza logica [p <-> q].

    Il risultato è true quando [p] e [q] hanno lo stesso valore di verità,
    altrimenti è [false]. *)

val xor : bool -> bool -> bool
(** [xor p q] calcola lo XOR (OR esclusivo) tra [p] e [q].

    Il risultato è [true] quando esattamente una delle due proposizioni è vera.
*)
