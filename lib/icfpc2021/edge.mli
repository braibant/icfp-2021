open! Core

type t = int * int [@@deriving sexp]

include Comparable.S with type t := t
