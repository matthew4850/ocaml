open! Cmm
open Reg
open Mach

let _stackp r =
  match r.loc with
    Stack _ -> true
  | _ -> false

class reload = object (_)

inherit Reloadgen.reload_generic as super

method! reload_operation op arg res =
  match op with
  | Imove | Ireload | Ispill ->
      (arg, res)
  | _ ->
      super#reload_operation op arg res

method! reload_test _ arg =
      arg
end

let fundecl f num_stack_slots =
  (new reload)#fundecl f num_stack_slots
