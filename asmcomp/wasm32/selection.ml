open! Cmm
open Arch
open! Mach

(* Instruction selection *)

class selector = object

inherit Selectgen.selector_generic as super

method is_immediate_test _cmp _n = false

method! is_immediate _op _n = false

method select_addressing _chunk arg = Iindexed 0, arg

method! select_operation op args dbg =
  super#select_operation op args dbg

end

let fundecl ~future_funcnames f =
  (new selector)#emit_fundecl ~future_funcnames f
