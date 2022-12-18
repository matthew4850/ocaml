open Misc
open Cmm
open Reg
open Arch
open Mach
(* Instruction selection *)

let word_addressed = false

(* Registers available for register allocation *)
let num_register_classes = 3

let register_class r =
  match r.typ with
  | Addr -> 0
  | Int | Val -> 1
  | Float -> 2

let num_available_registers = [| 50; 50; 50 |]

let first_available_register = [| 0; 1000; 2000 |]

let register_name r =
  Int.to_string r

let rotate_registers = true

let hard_addr_reg =
  Array.init 50 (fun i -> Reg.at_location Addr (Reg i))

let hard_int_reg =
  Array.init 50 (fun i -> Reg.at_location Int (Reg (1000 + i)))

let hard_float_reg =
  Array.init 50 (fun i -> Reg.at_location Float (Reg (2000 + i)))

let all_phys_regs =
  hard_addr_reg
  |> Array.append hard_int_reg
  |> Array.append hard_float_reg

let phys_reg n =
  if n < 1000 then
    hard_addr_reg.(n)
  else if n < 2000 then
    hard_int_reg.(n - 1000)
  else
    hard_float_reg.(n - 2000)

let stack_slot slot ty =
  Reg.at_location ty (Stack slot)

(* Calling conventions *)

let max_arguments_for_tailcalls = Int.max_int

let size_domainstate_args = 64 * size_int

let calling_conventions
    first_int32 last_int32 first_int64 last_int64 first_float
    last_float make_stack first_stack arg =
  let loc = Array.make (Array.length arg) Reg.dummy in
  let int32 = ref first_int32 in
  let int64 = ref first_int64 in
  let float = ref first_float in
  let ofs = ref first_stack in
  for i = 0 to Array.length arg - 1 do
    match arg.(i) with
    | Val | Addr ->
        if !int32 <= last_int32 then begin
          loc.(i) <- phys_reg !int32;
          incr int32
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) Val;
          ofs := !ofs + size_int
        end
    | Int as ty ->
        if !int64 <= last_int64 then begin
          loc.(i) <- phys_reg !int64;
          incr int64
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) ty;
          ofs := !ofs + size_int
        end
    | Float ->
        if !float <= last_float then begin
          loc.(i) <- phys_reg !float;
          incr float
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) Float;
          ofs := !ofs + size_float
        end
  done;
  (loc, Misc.align (max 0 !ofs) 16) (* Keep stack 16-aligned. *)

let incoming ofs =
  if ofs >= 0
  then Incoming ofs
  else Domainstate (ofs + size_domainstate_args)
let outgoing ofs =
  if ofs >= 0
  then Outgoing ofs
  else Domainstate (ofs + size_domainstate_args)
let not_supported _ofs = fatal_error "Proc.loc_results: cannot call"

let loc_arguments arg =
  calling_conventions 0 50 1000 1050 2000 2050 outgoing (- size_domainstate_args) arg

let loc_parameters arg =
  let (loc, _ofs) =
    calling_conventions 0 50 1000 1050 2000 2050 incoming (- size_domainstate_args) arg
  in
  loc

let loc_results res =
  let (loc, _ofs) =
    calling_conventions 0 50 1000 1050 2000 2050 not_supported 0 res
  in
  loc

let loc_external_arguments ty_args =
  let arg = Cmm.machtype_of_exttype_list ty_args in
  let loc, stack_ofs = calling_conventions 0 50 1000 1050 2000 2050 outgoing (- size_domainstate_args) arg in
  Array.map (fun reg -> [|reg|]) loc, stack_ofs

let loc_external_results res =
  let (loc, _ofs) =
    calling_conventions 0 50 1000 1050 2000 2050 not_supported 0 res
  in
  loc

(* Exceptions are in a0 *)

let loc_exn_bucket = hard_addr_reg.(0)

(* Volatile registers: none *)

let regs_are_volatile _ = false

(* Registers destroyed by operations *)

let _destroyed_at_alloc_or_poll =
  [| |]

let destroyed_at_oper = function
  | Iop(Icall_ind | Icall_imm _ | Iextcall { alloc = true; }) ->
    all_phys_regs
  | _ -> [||]

let destroyed_at_raise = [| |]

let destroyed_at_reloadretaddr = [| |]

(* Maximal register pressure *)

let safe_register_pressure _op = Int.max_int

let max_register_pressure _op = [| |]
(* Layout of the stack *)

let frame_required fd =
  fd.fun_contains_calls ||
  fd.fun_num_stack_slots.(0) > 0 || fd.fun_num_stack_slots.(1) > 0

let prologue_required fd =
  frame_required fd

let dwarf_register_numbers ~reg_class =
  ignore reg_class;
  failwith "not implemented 8"

let stack_ptr_dwarf_register_number = 0

(* Calling the assembler *)

let assemble_file infile outfile =
  Ccomp.command
    (Config.asm ^ " -sMEMORY64 -o " ^ Filename.quote outfile ^ " " ^ Filename.quote infile)

let init () = ()
