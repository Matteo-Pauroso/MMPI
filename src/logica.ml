(** [implica p q] calcola l'implicazione logica [p -> q].

    L'implicazione è falsa solamente quando [p] è vera e [q] è falsa. *)
let implica p q = (not p) || q

(** [se_e_solo_se] calcola l'equivalenza logica [p <-> q].

    Il risultato è true quando [p] e [q] hanno lo stesso valore di verità,
    altrimenti è [false]. *)
let se_e_solo_se p q = p = q

(** [xor p q] calcola lo XOR (OR esclusivo) tra [p] e [q].

    Il risultato è [true] quando esattamente una delle due proposizioni è vera.
*)
let xor p q = p <> q
