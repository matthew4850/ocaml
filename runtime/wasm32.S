#include "caml/m.h"

        .tagtype __ocaml_exception i64
        .global __ocaml_exception
        .globaltype __stack_pointer, i64
        .globaltype __caml_young_ptr, i64
        .globaltype __caml_state, i64

#define TEXT_SECTION(name) .section .text.caml.##name,"",@
#define FUNCTION(name) \
        TEXT_SECTION(name); \
        .globl name; \
        .type name,@function; \
        name:

#define END_FUNCTION(name) \
        end_function; \
        .size name, . - name

#define domain_field_caml_young_ptr 8
#define domain_field_caml_exception_pointer 16
#define domain_field_caml_bottom_of_stack 208 /* 8 * 26 */
#define domain_field_caml_last_return_address 216 /* 8 * 27 */
#define domain_field_caml_gc_regs 224 /* 8 * 28 */
#define domain_field_caml_backtrace_active 232 /* 8 * 29 */
#define Caml_state(var) domain_field_caml_##var

#define L(lbl) .L##lbl

.functype caml_garbage_collection () -> ()
FUNCTION(caml_call_gc)
        .functype caml_call_gc (i64, i64) -> ()

        global.get __caml_state
        local.get 0
        i64.store Caml_state(last_return_address)

        global.get __caml_state
        local.get 1
        i64.store Caml_state(bottom_of_stack)

        global.get __caml_state
        global.get __stack_pointer
        i64.store Caml_state(gc_regs)

        global.get __caml_state
        global.get __caml_young_ptr
        i64.store Caml_state(young_ptr)

        call caml_garbage_collection

        global.get __caml_state
        i64.load    Caml_state(young_ptr)
        global.set __caml_young_ptr
END_FUNCTION(caml_call_gc)
FUNCTION(caml_before_c_call)
        .functype caml_before_c_call (i64, i64) -> ()

        global.get __caml_state
        local.get 0
        i64.store Caml_state(last_return_address)

        global.get __caml_state
        local.get 1
        i64.store Caml_state(bottom_of_stack)

        global.get __caml_state
        global.get __caml_young_ptr
        i64.store    Caml_state(young_ptr)
END_FUNCTION(caml_before_c_call)

FUNCTION(caml_after_c_call)
        .functype caml_after_c_call () -> ()
        global.get __caml_state
        i64.load    Caml_state(young_ptr)
        global.set __caml_young_ptr
END_FUNCTION(caml_after_c_call)

.functype caml_program () -> (i64)

FUNCTION(prepare_for_ocaml)
        .functype prepare_for_ocaml (i64) -> ()

        local.get 0
        global.set __caml_state

        global.get __stack_pointer
        i64.const 40
        i64.sub
        local.tee 0
        global.set __stack_pointer

        local.get 0
        global.get __caml_state
        i64.load Caml_state(gc_regs)
        i64.store 24

        local.get 0
        global.get __caml_state
        i64.load Caml_state(last_return_address)
        i64.store 16

        local.get 0
        global.get __caml_state
        i64.load Caml_state(bottom_of_stack)
        i64.store 8

        global.get __caml_state
        i64.const    Caml_state(young_ptr)
        i64.add
        i64.load 0
        global.set __caml_young_ptr

        local.get 0
        i64.const 16
        i64.sub
        local.tee 0
        global.set __stack_pointer

        local.get 0
        global.get __caml_state
        i64.load Caml_state(exception_pointer)
        i64.store 0

        global.get __caml_state
        global.get __stack_pointer
        i64.store Caml_state(exception_pointer)

        local.get 0
        i64.const L(caml_retaddr)
        i64.store 0
END_FUNCTION(prepare_for_ocaml)

FUNCTION(cleanup_ocaml)
        .functype cleanup_ocaml () -> ()
        .local i64

        global.get __caml_state
        global.get __caml_young_ptr
        i64.store    Caml_state(young_ptr)

        global.get __stack_pointer
        local.tee 0
        i64.const 16
        i64.add
        local.tee 0
        global.set __stack_pointer

        global.get __caml_state
        local.get 0
        i64.load 8
        i64.store Caml_state(bottom_of_stack)

        global.get __caml_state
        local.get 0
        i64.load 16
        i64.store Caml_state(last_return_address)

        global.get __caml_state
        local.get 0
        i64.load 24
        i64.store Caml_state(gc_regs)

        global.get __stack_pointer
        i64.const 40
        i64.add
        local.tee 0
        global.set __stack_pointer

END_FUNCTION(cleanup_ocaml)

FUNCTION(caml_start_program)
        .functype caml_start_program (i64) -> (i64)
        .local i64

        local.get 0
        call prepare_for_ocaml

        try
        call caml_program
        local.set 0
        catch __ocaml_exception
        i64.const 2
        i64.or
        local.set 0
        end_try

        call cleanup_ocaml
        local.get 0
        return
END_FUNCTION(caml_start_program)

.section .data,"",@
L(caml_retaddr):
  .byte 0
  .size L(caml_retaddr), 1

.functype caml_stash_backtrace (i64, i64, i64, i64) -> ()
FUNCTION(caml_raise_exn)
        .functype caml_raise_exn (i64, i64) -> ()
        .local i64

        global.get __caml_state
        i64.load Caml_state(backtrace_active)
        i64.eqz
        i32.eqz
        if

        local.get 0

        local.get 1

        global.get __stack_pointer

        global.get __caml_state
        i64.load Caml_state(exception_pointer)

        call caml_stash_backtrace
        end_if

        global.get __caml_state
        i64.load Caml_state(exception_pointer)
        local.tee 2
        i64.const 16
        i64.add
        global.set __stack_pointer

        global.get __caml_state
        local.get 2
        i64.load 0
        i64.store Caml_state(exception_pointer)

        local.get 0
        throw __ocaml_exception
        unreachable
END_FUNCTION(caml_raise_exn)

/* Raise an exception from C */

FUNCTION(caml_raise_exception)
        .functype caml_raise_exception (i64, i64) -> ()
        local.get 0
        global.set __caml_state


        local.get 0
        i64.load Caml_state(backtrace_active)
        i64.eqz
        i32.eqz
        if
        local.get 1

        local.get 0
        i64.load Caml_state(last_return_address)

        local.get 0
        i64.load Caml_state(bottom_of_stack)

        local.get 0
        i64.load Caml_state(exception_pointer)

        call caml_stash_backtrace
        end_if

        global.get __caml_state
        i64.load Caml_state(exception_pointer)
        local.tee 0
        i64.const 16
        i64.add
        global.set __stack_pointer

        global.get __caml_state
        local.get 0
        i64.load 0
        i64.store Caml_state(exception_pointer)

        global.get __caml_state
        i64.load Caml_state(young_ptr)
        global.set __caml_young_ptr

        local.get 1
        throw __ocaml_exception
        unreachable

END_FUNCTION(caml_raise_exception)

/* Raise a Stack_overflow exception on return from segv_handler()
   (in runtime/signals_nat.c).  On entry, the stack is full, so we
   cannot record a backtrace.
   No CFI information here since this function disrupts the stack
   backtrace anyway. */

FUNCTION(caml_stack_overflow)
        .functype caml_stack_overflow () -> ()
        unreachable
END_FUNCTION(caml_stack_overflow)

/* Callback from C to OCaml */
FUNCTION(caml_callback_asm)
        .functype caml_callback_asm (i64, i64, i64) -> (i64)
        local.get 0
        call prepare_for_ocaml

        try
        local.get 2
        i64.load 0
        local.get 1
        local.get 1
        i64.load 0
        i32.wrap_i64
        call_indirect (i64, i64) -> (i64)
        local.set 0
        catch __ocaml_exception
        i64.const 2
        i64.or
        local.set 0
        end_try

        call cleanup_ocaml
        local.get 0
        return
END_FUNCTION(caml_callback_asm)

.functype caml_apply2 (i64, i64, i64) -> (i64)

FUNCTION(caml_callback2_asm)
        .functype caml_callback2_asm (i64, i64, i64) -> (i64)
        .local i64

        local.get 0
        call prepare_for_ocaml

        try
        local.get 2
        i64.load 0
        local.get 2
        i64.load 8
        local.get 1
        call caml_apply2
        local.set 0
        catch __ocaml_exception
        i64.const 2
        i64.or
        local.set 0
        end_try

        call cleanup_ocaml
        local.get 0
        return
END_FUNCTION(caml_callback2_asm)

.functype caml_apply3 (i64, i64, i64, i64) -> (i64)
FUNCTION(caml_callback3_asm)
        .functype caml_callback3_asm (i64, i64, i64) -> (i64)
        local.get 0
        call prepare_for_ocaml

        try
        local.get 2
        i64.load 0
        local.get 2
        i64.load 8
        local.get 2
        i64.load 16
        local.get 1
        call caml_apply3
        local.set 0
        catch __ocaml_exception
        i64.const 2
        i64.or
        local.set 0
        end_try

        call cleanup_ocaml
        local.get 0
        return
END_FUNCTION(caml_callback3_asm)

.functype caml_array_bound_error () -> ()
FUNCTION(caml_ml_array_bound_error)
        .functype caml_ml_array_bound_error (i64) -> ()
        local.get 0
        global.get __stack_pointer
        call caml_before_c_call
        call caml_array_bound_error
        call caml_after_c_call
END_FUNCTION(caml_ml_array_bound_error)

        .section .data,"",@
        .globl  __caml_young_ptr
__caml_young_ptr:
        .quad 0
        .size __caml_young_ptr, 8

        .section .data,"",@
        .globl  __caml_state
__caml_state:
        .quad 0
        .size __caml_state, 8

        .section .data,"",@
        .globl  caml_system__code_begin
caml_system__code_begin:
        .quad 0  /* just one instruction, so that debuggers don't display
        caml_system__code_begin instead of caml_call_gc */
        .size   caml_system__code_begin, .-caml_system__code_begin

        .section .data,"",@
        .globl  caml_system__code_end
caml_system__code_end:
        .size   caml_system__code_end, .-caml_system__code_end

        .section .data,"",@
        .globl  caml_system__frametable
        .type   caml_system__frametable, @object
        .align  8
caml_system__frametable:
        .quad   1               /* one descriptor */
        .quad   L(caml_retaddr) /* return address into callback */
        .short  -1              /* negative frame size => use callback link */
        .short  0               /* no roots */
        .size   caml_system__frametable, .-caml_system__frametable

__ocaml_exception:
