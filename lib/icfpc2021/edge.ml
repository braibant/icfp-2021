open! Core

module T = struct
  type t = int * int [@@deriving compare, sexp]
end

include T
include Comparable.Make (T)
