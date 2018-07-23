(** CSV files *)

open Core

type t = Csv_record.t Lazy_list.t

(** (lazily) enforces that all records are the same length *)
val read : Lexing.lexbuf -> t

(** enforces that all records are the same length *)
val write : ?sep:char -> Out_channel.t -> t -> unit
