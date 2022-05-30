type args = string list
(** Must be [string], because we could have big integers. *)

type t = {
  opcode: Opcode.opcode;
  args : args
}

val mk : Opcode.opcode -> args -> t
(** [mk opcode args] *)

val to_string : t -> string
