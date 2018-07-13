(** State module types *)

module type SINT63 = Interface.STATE with type raw := int


(** State implementations *)

module Int63Hamt : SINT63 = Int63Hamt


(** Functors *)

module FInt63 : functor (S : SINT63) -> Interface.MT = FInt63.Make



module Xkcd : Interface.MT = Xkcd
