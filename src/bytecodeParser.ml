open IStd
open Sedlexing

exception SyntaxError of int

let digit = [%sedlex.regexp? '0'..'9']
let hex = [%sedlex.regexp? digit | 'a' .. 'f' | 'A' .. 'F']
let white_spaces = [%sedlex.regexp? Star white_space]

let invalid_code = "fe"

let parse_two_hex_bytes n buf =
  let rec parse_two_hex_bytes n acc =
    if n <= 0 then acc
    else
      match%sedlex buf with
      | Rep (hex, 2) -> parse_two_hex_bytes (n - 1) (acc ^ Latin1.lexeme buf)
      | eof -> invalid_code
      | _ -> raise (SyntaxError (lexeme_start buf))
  in
  parse_two_hex_bytes n "0x"

let parse_hex (buf : Sedlexing.lexbuf) : Instruction.t list =
  let open Opcode in
  let rec parse_aux (acc : Instruction.t list) =
    match%sedlex buf with
    | "00" -> parse_aux (Instruction.mk STOP [] :: acc)
    | "01" -> parse_aux (Instruction.mk ADD [] :: acc)
    | "02" -> parse_aux (Instruction.mk MUL [] :: acc)
    | "03" -> parse_aux (Instruction.mk SUB [] :: acc)
    | "04" -> parse_aux (Instruction.mk DIV [] :: acc)
    | "05" -> parse_aux (Instruction.mk SDIV [] :: acc)
    | "06" -> parse_aux (Instruction.mk MOD [] :: acc)
    | "07" -> parse_aux (Instruction.mk SMOD [] :: acc)
    | "08" -> parse_aux (Instruction.mk ADDMOD [] :: acc)
    | "09" -> parse_aux (Instruction.mk MULMOD [] :: acc)
    | "0a" -> parse_aux (Instruction.mk EXP [] :: acc)
    | "0b" -> parse_aux (Instruction.mk SIGNEXTEND [] :: acc)
    | "10" -> parse_aux (Instruction.mk LT [] :: acc)
    | "11" -> parse_aux (Instruction.mk GT [] :: acc)
    | "12" -> parse_aux (Instruction.mk SLT [] :: acc)
    | "13" -> parse_aux (Instruction.mk SGT [] :: acc)
    | "14" -> parse_aux (Instruction.mk EQ [] :: acc)
    | "15" -> parse_aux (Instruction.mk ISZERO [] :: acc)
    | "16" -> parse_aux (Instruction.mk AND [] :: acc)
    | "17" -> parse_aux (Instruction.mk OR [] :: acc)
    | "18" -> parse_aux (Instruction.mk XOR [] :: acc)
    | "19" -> parse_aux (Instruction.mk NOT [] :: acc)
    | "1a" -> parse_aux (Instruction.mk BYTE [] :: acc)
    | "1b" -> parse_aux (Instruction.mk SHL [] :: acc)
    | "1c" -> parse_aux (Instruction.mk NOT [] :: acc)
    | "1d" -> parse_aux (Instruction.mk SAR [] :: acc)
    | "20" -> parse_aux (Instruction.mk SHA3 [] :: acc)
    | "30" -> parse_aux (Instruction.mk ADDRESS [] :: acc)
    | "31" -> parse_aux (Instruction.mk BALANCE [] :: acc)
    | "32" -> parse_aux (Instruction.mk ORIGIN [] :: acc)
    | "33" -> parse_aux (Instruction.mk CALLER [] :: acc)
    | "34" -> parse_aux (Instruction.mk CALLVALUE [] :: acc)
    | "35" -> parse_aux (Instruction.mk CALLDATALOAD [] :: acc)
    | "36" -> parse_aux (Instruction.mk CALLDATASIZE [] :: acc)
    | "37" -> parse_aux (Instruction.mk CALLDATACOPY [] :: acc)
    | "38" -> parse_aux (Instruction.mk CODESIZE [] :: acc)
    | "39" -> parse_aux (Instruction.mk CODECOPY [] :: acc)
    | "3a" -> parse_aux (Instruction.mk GASPRICE [] :: acc)
    | "3b" -> parse_aux (Instruction.mk EXTCODESIZE [] :: acc)
    | "3c" -> parse_aux (Instruction.mk EXTCODECOPY [] :: acc)
    | "3d" -> parse_aux (Instruction.mk RETURNDATASIZE [] :: acc)
    | "3e" -> parse_aux (Instruction.mk RETURNDATACOPY [] :: acc)
    | "3f" -> parse_aux (Instruction.mk EXTCODEHASH [] :: acc)
    | "40" -> parse_aux (Instruction.mk BLOCKHASH [] :: acc)
    | "41" -> parse_aux (Instruction.mk COINBASE [] :: acc)
    | "42" -> parse_aux (Instruction.mk TIMESTAMP [] :: acc)
    | "43" -> parse_aux (Instruction.mk NUMBER [] :: acc)
    | "44" -> parse_aux (Instruction.mk DIFFICULTY [] :: acc)
    | "45" -> parse_aux (Instruction.mk GASLIMIT [] :: acc)
    | "46" -> parse_aux (Instruction.mk CHAINID [] :: acc)
    | "47" -> parse_aux (Instruction.mk SELFBALANCE [] :: acc)
    | "48" -> parse_aux (Instruction.mk BASEFEE [] :: acc)
    | "50" -> parse_aux (Instruction.mk POP [] :: acc)
    | "51" -> parse_aux (Instruction.mk MLOAD [] :: acc)
    | "52" -> parse_aux (Instruction.mk MSTORE [] :: acc)
    | "53" -> parse_aux (Instruction.mk MSTORE8 [] :: acc)
    | "54" -> parse_aux (Instruction.mk SSTORE [] :: acc)
    | "55" -> parse_aux (Instruction.mk SSTORE [] :: acc)
    | "56" -> parse_aux (Instruction.mk JUMP [] :: acc)
    | "57" -> parse_aux (Instruction.mk JUMPI [] :: acc)
    | "58" -> parse_aux (Instruction.mk PC [] :: acc)
    | "59" -> parse_aux (Instruction.mk MSIZE [] :: acc)
    | "5a" -> parse_aux (Instruction.mk GAS [] :: acc)
    | "5b" -> parse_aux (Instruction.mk JUMPDEST [] :: acc)
    | "5b" -> parse_aux (Instruction.mk JUMPDEST [] :: acc)
    | ('6' | '7'), hex -> begin
        let n = Int.of_string ("0x" ^ Latin1.lexeme buf) - (0x60 - 1) in
        let push = Opcode.mk_push_exn n in
        parse_aux (Instruction.mk push [parse_two_hex_bytes n buf] :: acc)
      end
    | '8', hex -> begin
        let n = Int.of_string ("0x" ^ Latin1.lexeme buf) - (0x80 - 1) in
        let dup = Opcode.mk_dup_exn n in
        parse_aux (Instruction.mk dup [] :: acc)
      end
    | '9', hex -> begin
        let n = Int.of_string ("0x" ^ Latin1.lexeme buf) - (0x90 - 1) in
        let swap = Opcode.mk_swap_exn n in
        parse_aux (Instruction.mk swap [] :: acc)
      end
    | "a0" -> parse_aux (Instruction.mk LOG0 [] :: acc)
    | "a1" -> parse_aux (Instruction.mk LOG1 [] :: acc)
    | "a2" -> parse_aux (Instruction.mk LOG2 [] :: acc)
    | "a3" -> parse_aux (Instruction.mk LOG3 [] :: acc)
    | "a4" -> parse_aux (Instruction.mk LOG4 [] :: acc)
    | "f0" -> parse_aux (Instruction.mk CREATE [] :: acc)
    | "f1" -> parse_aux (Instruction.mk CALL [] :: acc)
    | "f2" -> parse_aux (Instruction.mk CALLCODE [] :: acc)
    | "f3" -> parse_aux (Instruction.mk RETURN [] :: acc)
    | "f4" -> parse_aux (Instruction.mk DELEGATECALL [] :: acc)
    | "f5" -> parse_aux (Instruction.mk CREATE2 [] :: acc)
    | "fa" -> parse_aux (Instruction.mk STATICCALL [] :: acc)
    | "fd" -> parse_aux (Instruction.mk REVERT [] :: acc)
    | "fe" -> parse_aux (Instruction.mk INVALID [] :: acc)
    | "ff" -> parse_aux (Instruction.mk SELFDESTRUCT [] :: acc)
    | white_spaces, eof -> acc
    | _ ->
      (* Most likely, this is a part of the metadata section:
         https://docs.soliditylang.org/en/v0.8.14/metadata.html *)
      let _ = parse_two_hex_bytes 1 buf in
      (* Printf.printf "Unknown: %s (pos=%d)\n" *)
      (*      (parse_two_hex_bytes 1 buf)           *)
      (*      (lexeme_start buf);               *)
      parse_aux (Instruction.mk INVALID [] :: acc)
  in
  parse_aux [] |> List.rev

let parse filename =
  let buf = Sedlexing.Latin1.from_channel (In_channel.create filename) in
  match%sedlex buf with
  | "0x" -> parse_hex buf
  | hex -> rollback buf; parse_hex buf
  | white_spaces, eof -> []
  | _ -> failwith "Impossible"

