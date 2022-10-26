open Format
open! Array

(* Machine-specific command-line options *)

let command_line_options = []

(* Specific operations *)

type specific_operation

(* Addressing modes *)

type addressing_mode =
  | Iindexed of int

let is_immediate _n = true

(* Sizes, endianness *)

let big_endian = false

let size_addr = 4
let size_int = 8
let size_float = 8

let allow_unaligned_access = true

(* Behavior of division *)

let division_crashes_on_overflow = true

(* Operations on addressing modes *)

let identity_addressing = Iindexed 0

let offset_addressing addr delta =
  match addr with
  | Iindexed n -> Iindexed(n + delta)

(* Printing operations and addressing modes *)

let print_addressing printreg addr ppf arg =
  match addr with
  | Iindexed n ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a%s" printreg arg.(0) idx

let print_specific_operation _printreg op _ppf _arg =
  match op with
  | _ -> assert false

(* Specific operations that are pure *)

let operation_is_pure _ = true

(* Specific operations that can raise *)

let operation_can_raise _ = false
