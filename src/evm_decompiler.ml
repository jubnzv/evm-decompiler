open IStd

let print_instructions bc =
  List.iter bc
    ~f:(fun (instr : Instruction.t) ->
        Instruction.to_string instr |> Printf.printf "%s\n")

let () =
  Clap.description "An analyzer";
  let files =
    Clap.list_string
      ~description:
        "Paths to source files to check"
      ~placeholder: "PATHS"
      ()
  in
  Clap.close ();

  let bc = List.hd_exn files |> BytecodeParser.parse in
  print_instructions bc
