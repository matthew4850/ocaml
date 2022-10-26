(* Instruction selection *)

let word_addressed = false

(* Registers available for register allocation *)
let num_register_classes = 0

let register_class _r =
  failwith "not implemented"

let num_available_registers = [| |]

let first_available_register = [| |]

let register_name r =
  Int.to_string r

let rotate_registers = true

let phys_reg _n =
  failwith "not implemented"

(* Calling conventions *)

let max_arguments_for_tailcalls = Int.max_int

let loc_arguments _arg =
  failwith "not implemented"

let loc_parameters _arg =
  failwith "not implemented"

let loc_results _res =
  failwith "not implemented"

let loc_external_arguments _ty_args =
  failwith "not implemented"

let loc_external_results _res =
  failwith "not implemented"

(* Exceptions are in a0 *)

let loc_exn_bucket = Reg.dummy

(* Volatile registers: none *)

let regs_are_volatile _ = false

(* Registers destroyed by operations *)

let _destroyed_at_c_call =
  [| |]

let _destroyed_at_alloc =
  [| |]

let destroyed_at_oper _op = [| |]

let destroyed_at_raise = [| |]

let destroyed_at_reloadretaddr = [| |]

(* Maximal register pressure *)

let safe_register_pressure _op = Int.max_int

let max_register_pressure _op = [| |]
(* Layout of the stack *)

let frame_required _fd = false

let prologue_required _fd = false

let dwarf_register_numbers ~reg_class =
  ignore reg_class;
  failwith "not implemented"

let stack_ptr_dwarf_register_number = 0

(* Calling the assembler *)

let assemble_file _infile _outfile =
  failwith "not implemented"

let init () = ()
