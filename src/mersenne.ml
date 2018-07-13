(** State module types *)

module type SINT63 = Interface.STATE with type raw := int


(** State implementations *)

module Int63Hamt : SINT63 = Int63Hamt
module Int63Map : SINT63 = Int63Map
module Int63Array : SINT63 = Int63Array


(** Functors *)

module FInt63 : functor (S : SINT63) -> Interface.MT = FInt63.Make



module Xkcd : Interface.MT = Xkcd
