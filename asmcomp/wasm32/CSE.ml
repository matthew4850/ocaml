open! Arch
open Mach
open CSEgen

class cse = object (_self)

inherit cse_generic as super

method! class_of_operation op =
  match op with
  | Ispecific(_) -> assert false
  | _ -> super#class_of_operation op

end

let fundecl f =
  (new cse)#fundecl f
