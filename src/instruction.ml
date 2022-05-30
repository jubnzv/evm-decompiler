open IStd

type args = string list

type t = {
  opcode: Opcode.opcode;
  args: args
}

let mk opcode args =
  { opcode; args }

let to_string instr =
  String.concat ~sep:" " instr.args
  |> Printf.sprintf "%s %s" @@ Opcode.show_opcode instr.opcode
